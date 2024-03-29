package org.geneontology.jena

import com.typesafe.scalalogging.StrictLogging

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import org.apache.jena.reasoner.rulesys.Rule
import org.geneontology.jena.SWRLUtil._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model._

import scala.collection.JavaConverters._
import scala.collection.parallel.immutable.ParSet

object OWLtoRules extends StrictLogging {

  val IndirectType = "http://arachne.geneontology.org/indirect_type"

  private val factory = OWLManager.getOWLDataFactory

  def translate(ont: OWLOntology, includeImportsClosure: Imports, translateTbox: Boolean, translateRbox: Boolean, translateAbox: Boolean, translateRules: Boolean): Set[Rule] = {
    var axioms = ParSet.empty[OWLAxiom]
    if (translateTbox) axioms ++= ont.getTBoxAxioms(includeImportsClosure).asScala
    if (translateRbox) axioms ++= ont.getRBoxAxioms(includeImportsClosure).asScala
    if (translateAbox) axioms ++= ont.getABoxAxioms(includeImportsClosure).asScala
    if (translateRules) axioms ++= ont.getAxioms(AxiomType.SWRL_RULE, includeImportsClosure).asScala.toSet[OWLAxiom]
    axioms.flatMap(translateAxiom).seq ++ builtInRules
  }

  def translateAxiom(axiom: OWLAxiom): Set[Rule] = axiom match {

    case SubClassOf(_, subClass, superClass) if subClass != OWLNothing =>
      translateAxiom(subClass('x) --> superClass('x))

    case EquivalentClasses(_, operands) => for {
      superClass <- operands
      subClass <- operands.filterNot(_ == superClass) //FIXME scowl types ought to allow minus
      rule <- translateAxiom(SubClassOf(subClass, superClass))
    } yield rule

    case DisjointClasses(_, operands) => (for {
      pair <- operands.toSeq.combinations(2)
      rule <- translateAxiom(SubClassOf(ObjectIntersectionOf(pair.toSet), OWLNothing))
    } yield rule).toSet

    case ClassAssertion(_, Class(cls), NamedIndividual(ind)) => Set(Rule.parseRule(s"[ -> (<$ind> rdf:type <$cls>) ]"))

    case ClassAssertion(_, ObjectHasValue(prop, NamedIndividual(objIRI)), NamedIndividual(subjIRI)) =>
      val subj = s"<$subjIRI>"
      val obj = s"<$objIRI>"
      Set(Rule.parseRule(s"[ -> ${rel(subj, prop, obj)} ]"))

    case ClassAssertion(_, ce, ind @ NamedIndividual(_)) => translateAxiom(SubClassOf(ObjectOneOf(ind), ce))

    case ObjectPropertyAssertion(_, prop, NamedIndividual(subjIRI), NamedIndividual(objIRI)) =>
      val subj = s"<$subjIRI>"
      val obj = s"<$objIRI>"
      Set(Rule.parseRule(s"[ -> ${rel(subj, prop, obj)} ]"))

    case ObjectPropertyDomain(_, p, ce) => translateAxiom(SubClassOf((p some OWLThing), ce))

    case ObjectPropertyRange(_, p, ce) => translateAxiom(SubClassOf((p.getInverseProperty some OWLThing), ce))

    case SubObjectPropertyOf(_, p, q) =>
      val (x1, x2) = ("?x1", "?x2")
      Set(Rule.parseRule(s"[ ${rel(x1, p, x2)} -> ${rel(x1, q, x2)} ]"))

    case EquivalentObjectProperties(_, operands) => for {
      superProp <- operands
      subProp <- operands - superProp
      rule <- translateAxiom(SubObjectPropertyOf(subProp, superProp))
    } yield rule

    case DisjointObjectProperties(_, operands) => (for {
      pair <- operands.toSeq.combinations(2)
      p = pair(0)
      q = pair(1)
    } yield {
      val (x1, x2) = ("?x1", "?x2")
      Rule.parseRule(s"[ ${rel(x1, p, x2)} ${rel(x1, q, x2)} -> ($x1 rdf:type owl:Nothing) ($x2 rdf:type owl:Nothing) ]")
    }).toSet

    case InverseObjectProperties(_, p, q) =>
      val (x1, x2) = ("?x1", "?x2")
      Set(Rule.parseRule(s"[ ${rel(x1, p, x2)} -> ${rel(x2, q, x1)} ]"),
        Rule.parseRule(s"[ ${rel(x1, q, x2)} -> ${rel(x2, p, x1)} ]"))

    case FunctionalObjectProperty(_, p) =>
      val (x1, x2, x3) = ("?x1", "?x2", "?x3")
      Set(Rule.parseRule(s"[ ${rel(x1, p, x2)} ${rel(x1, p, x3)} -> ($x2 owl:sameAs $x3) ]"))

    case InverseFunctionalObjectProperty(_, p) =>
      val (x1, x3, x2) = ("?x1", "?x3", "?x2")
      Set(Rule.parseRule(s"[ ${rel(x1, p, x2)} ${rel(x3, p, x2)} -> ($x1 owl:sameAs $x3) ]"))

    case IrreflexiveObjectProperty(_, p) =>
      val x = "?x"
      Set(Rule.parseRule(s"[ ${rel(x, p, x)} -> ($x rdf:type owl:Nothing) ]"))

    case SymmetricObjectProperty(_, p) =>
      val (x, y) = ("?x1", "?x2")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} -> ${rel(y, p, x)} ]"))

    case AsymmetricObjectProperty(_, p) =>
      val (x, y) = ("?x1", "?x2")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} ${rel(y, p, x)} -> ($x rdf:type owl:Nothing) ($y rdf:type owl:Nothing) ]"))

    case TransitiveObjectProperty(_, p) =>
      val (x, y, z) = ("?x1", "?x2", "?x3")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} ${rel(y, p, z)} -> ${rel(x, p, z)} ]"))

    case SubObjectPropertyChainOf(_, subprops, prop) =>
      val start = makeSubject(0)
      val end = makeSubject(subprops.size)
      val patterns = for {
        (subprop, index) <- subprops.zipWithIndex
      } yield rel(makeSubject(index), subprop, makeSubject(index + 1))
      Set(Rule.parseRule(s"[ ${patterns.mkString(" ")} -> ${rel(start, prop, end)} ]"))

    case SameIndividual(_, operands) => (for {
      pair <- operands.toSeq.combinations(2)
      NamedIndividual(p) = pair(0)
      NamedIndividual(q) = pair(1)
    } yield {
      Rule.parseRule(s"[ -> (<$p> owl:sameAs <$q>) ]")
    }).toSet

    case DifferentIndividuals(_, operands) => (for {
      pair <- operands.toSeq.combinations(2)
      NamedIndividual(p) = pair(0)
      NamedIndividual(q) = pair(1)
    } yield {
      Rule.parseRule(s"[ -> (<$p> owl:differentFrom <$q>) ]")
    }).toSet

    //TODO data properties, abox axioms

    case rule: SWRLRule => translateSWRLRule(rule)

    case _ => {
      logger.debug("Skipping: " + axiom)
      Set.empty
    }

  }

  private def builtInRules: Set[Rule] = Set(
    "[ (?a owl:sameAs ?b) -> (?b owl:sameAs ?a) ]",
    "[ (?a owl:sameAs ?b) (?b owl:sameAs ?c) notEqual(?a, ?c) -> (?a owl:sameAs ?c) ]",
    "[ (?a owl:differentFrom ?b) -> (?b owl:differentFrom ?a) ]",
    "[ (?a owl:sameAs ?b) (?a owl:differentFrom ?b) -> (?a rdf:type owl:Nothing) (?b rdf:type owl:Nothing) ]",
    "[ (?a owl:sameAs ?b) (?a ?p ?o) notEqual(?p, owl:sameAs) -> (?b ?p ?o) ]",
    "[ (?a owl:sameAs ?b) (?s ?p ?a) notEqual(?p, owl:sameAs) -> (?s ?p ?b) ]")
    .map(Rule.parseRule)

  private def makeRule(body: Intersection, head: String): Rule = {
    val sortedAtoms = body.atoms.toSeq.sortWith((a, b) => a.startsWith("(?x "))
    Rule.parseRule(s"[ ${sortedAtoms.mkString(" ")} -> $head ]")
  }

  private def rel(subj: String, ope: OWLObjectPropertyExpression, obj: String): String = ope match {
    case ObjectProperty(prop)                  => s"($subj <$prop> $obj)"
    case ObjectInverseOf(ObjectProperty(prop)) => s"($obj <$prop> $subj)"
  }

  private def translateSWRLRule(swrl: SWRLRule): Set[Rule] = {
    val incrementer = new AtomicInteger(0)
    for {
      simplified <- simplifySWRLHead(swrl)
      variables = mapSWRLVariables(simplified, incrementer)
      ruleHead <- simplified.getHead.asScala.toSet[SWRLAtom]
      jenaRule <- translateBodyAtom(ruleHead, variables, incrementer) match {
        case Intersection(atoms) if atoms.size == 1 =>
          val jenaHead = atoms.head
          simplified.getBody.asScala.toSet[SWRLAtom].map(translateBodyAtom(_, variables, incrementer)).fold(NoAtoms)(combine) match {
            case Union(intersections)           => intersections.map(makeRule(_, jenaHead))
            case intersection @ Intersection(_) => Set(makeRule(intersection, jenaHead))
            case NoAtoms                        => Set(makeRule(Intersection(Nil), jenaHead))
            case InvalidAtoms                   => Set.empty[Rule]
          }
        case _                                      => Set.empty[Rule]
      }
    } yield jenaRule
  }

  private def simplifySWRLHead(swrl: SWRLRule): Set[SWRLRule] = {
    val headAtoms = swrl.getHead.asScala.toSet[SWRLAtom]
    if (headAtoms.size > 1) (for {
      headAtom <- headAtoms
      simplified <- simplifySWRLHead(factory.getSWRLRule(swrl.getBody, Set(headAtom).asJava))
    } yield simplified).toSet
    else if (headAtoms.size == 1)
      headAtoms.head match {
        case ObjectPropertyAtom(_, _, _)                       => Set(swrl)
        case SameIndividualAtom(_, _)                          => Set(swrl)
        case DifferentIndividualsAtom(_, _)                    => Set(swrl)
        case ClassAtom(Class(_), _)                            => Set(swrl)
        case ClassAtom(ObjectIntersectionOf(operands), arg)    => for {
          operand <- operands
          clsAtom <- Set(factory.getSWRLClassAtom(operand, arg))
          newRule = factory.getSWRLRule(swrl.getBody, Set(clsAtom).asJava)
          simplified <- simplifySWRLHead(newRule)
        } yield simplified
        case ClassAtom(ObjectAllValuesFrom(prop, filler), arg) =>
          val obj = freshSWRLVariable
          simplifySWRLHead(factory.getSWRLRule(
            (swrl.getBody.asScala.toSet[SWRLAtom] + ObjectPropertyAtom(prop, arg, obj)).asJava,
            Set(ClassAtom(filler, obj)).asJava))
        case ClassAtom(ObjectComplementOf(ce), arg)            => simplifySWRLHead(factory.getSWRLRule(
          (swrl.getBody.asScala.toSet[SWRLAtom] + ClassAtom(ce, arg)).asJava,
          Set(ClassAtom(OWLNothing, arg)).asJava))
        case ClassAtom(ObjectHasValue(pe, ind), arg)           => simplifySWRLHead(factory.getSWRLRule(
          swrl.getBody,
          Set(ObjectPropertyAtom(pe, arg, IndividualArg(ind))).asJava))
        case ClassAtom(ObjectMaxCardinality(max, pe, ce), arg) => max match {
          case 0 =>
            val obj = freshSWRLVariable
            val fillerAtom = if (ce != OWLThing) Set(ce(obj)) else Set.empty
            simplifySWRLHead(factory.getSWRLRule(
              (swrl.getBody.asScala.toSet[SWRLAtom] ++ fillerAtom + ObjectPropertyAtom(pe, arg, obj)).asJava,
              Set(ClassAtom(OWLNothing, arg)).asJava))
          case 1 =>
            val obj1 = freshSWRLVariable
            val obj2 = freshSWRLVariable
            val fillerAtoms = if (ce != OWLThing) Set(ce(obj1), ce(obj2)) else Set.empty
            simplifySWRLHead(factory.getSWRLRule(
              (swrl.getBody.asScala.toSet[SWRLAtom] ++ fillerAtoms + ObjectPropertyAtom(pe, arg, obj1) + ObjectPropertyAtom(pe, arg, obj2)).asJava,
              Set(SameIndividualAtom(obj1, obj2)).asJava))
          case _ => Set.empty // unsupported cardinality
        }
        case _                                                 => Set.empty // unsupported head
      }
    else Set.empty // no head //FIXME log
  }

  private def translateBodyAtom(atom: SWRLAtom, variables: Map[IRI, String], incrementer: AtomicInteger): Atoms = atom match {
    case ObjectPropertyAtom(pe, subj, obj)    => (for {
      subjNode <- translateSWRLArgument(subj, variables)
      objNode <- translateSWRLArgument(obj, variables)
    } yield Intersection(List(rel(subjNode, pe, objNode)))).getOrElse(InvalidAtoms)
    case SameIndividualAtom(arg1, arg2)       => (for {
      subjNode <- translateSWRLArgument(arg1, variables)
      objNode <- translateSWRLArgument(arg2, variables)
    } yield Intersection(List(s"($subjNode owl:sameAs $objNode)"))).getOrElse(InvalidAtoms)
    case DifferentIndividualsAtom(arg1, arg2) => (for {
      subjNode <- translateSWRLArgument(arg1, variables)
      objNode <- translateSWRLArgument(arg2, variables)
    } yield Intersection(List(s"($subjNode owl:differentFrom $objNode)"))).getOrElse(InvalidAtoms)
    case ClassAtom(ce, arg)                   => translateSWRLArgument(arg, variables).map(translateExpression(ce, _, incrementer))
      .getOrElse(InvalidAtoms)
  }

  private def translateExpression(ce: OWLClassExpression, subject: String, incrementer: AtomicInteger): Atoms = ce match {
    case OWLThing                                       => NoAtoms
    case Class(iri)                                     => Intersection(List(s"($subject rdf:type <$iri>)"))
    case ObjectSomeValuesFrom(property, filler)         =>
      val nextSubject = makeSubject(incrementer.incrementAndGet())
      val triple = Intersection(List(rel(subject, property, nextSubject)))
      combine(triple, translateExpression(filler, nextSubject, incrementer))
    case ObjectIntersectionOf(operands)                 => operands.map(translateExpression(_, subject, incrementer))
      .fold(NoAtoms)(combine)
    case ObjectUnionOf(operands)                        => operands.map(translateExpression(_, subject, incrementer))
      .foldLeft(Union(Set.empty))(combineIntoUnion)
    case ObjectHasValue(property, NamedIndividual(ind)) => Intersection(List(rel(subject, property, s"<$ind>")))
    case ObjectOneOf(individuals)                       => Union(for {
      NamedIndividual(ind) <- individuals
    } //FIXME this probably wouldn't perform very well but if this is only atom rule won't match without triple pattern
      yield Intersection(List(s"($subject ?pred ?obj) equal($subject, <$ind>)")))
    case _                                              => InvalidAtoms // Invalid class expression used in body
  }

  private sealed trait Atoms

  private case class Intersection(atoms: List[String]) extends Atoms

  private case class Union(intersections: Set[Intersection]) extends Atoms

  private case object NoAtoms extends Atoms

  private case object InvalidAtoms extends Atoms

  private def combine(a: Atoms, b: Atoms): Atoms = (a, b) match {
    case (i @ Intersection(_), Union(us))   => Union(us.map(intersection => Intersection(i.atoms ::: intersection.atoms)))
    case (Union(us), i @ Intersection(_))   => Union(us.map(intersection => Intersection(i.atoms ::: intersection.atoms)))
    case (Intersection(a), Intersection(b)) => Intersection(a ::: b)
    case (Union(a), Union(b))               => Union(for {
      ai <- a
      bi <- b
    } yield Intersection(ai.atoms ++ bi.atoms))
    case (other, NoAtoms)                   => other
    case (NoAtoms, other)                   => other
    case (_, InvalidAtoms)                  => InvalidAtoms
    case (InvalidAtoms, _)                  => InvalidAtoms
  }

  private def combineIntoUnion(union: Union, atoms: Atoms): Union = atoms match {
    case i @ Intersection(_) => Union(union.intersections + i)
    case Union(is)           => Union(union.intersections ++ is)
    case NoAtoms             => Union(union.intersections + Intersection(Nil))
    case InvalidAtoms        => union
  }

  private def makeSubject(level: Int): String = if (level == 0) "?x" else s"?x$level"

  private def translateSWRLArgument(swrlArg: SWRLIArgument, varMap: Map[IRI, String]): Option[String] = swrlArg match {
    case IndividualArg(NamedIndividual(iri))   => Option(s"<$iri>")
    case IndividualArg(AnonymousIndividual(_)) => None
    case Variable(iri)                         => varMap.get(iri)
  }

  private def mapSWRLVariables(rule: SWRLRule, incrementer: AtomicInteger): Map[IRI, String] =
    (for {
      variable <- rule.getVariables.asScala.toSet[SWRLVariable]
    } yield {
      variable.getIRI -> makeSubject(incrementer.incrementAndGet())
    }).toMap

  private def freshSWRLVariable: SWRLVariable = factory.getSWRLVariable(IRI.create(s"urn:uuid:${UUID.randomUUID}"))

  def indirectRules(ontology: OWLOntology): Set[Rule] = (for {
    axiom <- ontology.getAxioms(AxiomType.SUBCLASS_OF, Imports.INCLUDED).asScala.toSet[OWLSubClassOfAxiom]
    superclass = axiom.getSuperClass
    subclass = axiom.getSubClass
    if subclass != superclass
    if !subclass.isAnonymous
    if !superclass.isAnonymous
  } yield Rule.parseRule(s"[ (?x rdf:type <${subclass.asOWLClass.getIRI}>) (?x rdf:type <${superclass.asOWLClass.getIRI}>) -> (?x <$IndirectType> <${superclass.asOWLClass.getIRI}>) ]")).toSet

}