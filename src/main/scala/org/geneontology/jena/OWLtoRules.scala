package org.geneontology.jena

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.JavaConverters._
import scala.collection.parallel.immutable.ParSet

import org.apache.jena.reasoner.rulesys.Rule
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports

import com.typesafe.scalalogging.LazyLogging

object OWLtoRules extends LazyLogging {

  def translate(ont: OWLOntology, includeImportsClosure: Imports, translateTbox: Boolean, translateRbox: Boolean, translateAbox: Boolean): Set[Rule] = {
    var axioms = ParSet.empty[OWLAxiom]
    if (translateTbox) axioms ++= ont.getTBoxAxioms(includeImportsClosure).asScala
    if (translateRbox) axioms ++= ont.getRBoxAxioms(includeImportsClosure).asScala
    if (translateAbox) axioms ++= ont.getABoxAxioms(includeImportsClosure).asScala
    axioms.flatMap(translateAxiom).seq
  }

  def translateAxiom(axiom: OWLAxiom): Set[Rule] = axiom match {

    case SubClassOf(_, ObjectUnionOf(operands), superClass) =>
      operands.flatMap(ce => translateAxiom(SubClassOf(ce, superClass)))

    case SubClassOf(_, subClass, ObjectIntersectionOf(operands)) => for {
      operand <- operands
      rule <- translateAxiom(SubClassOf(subClass, operand))
    } yield rule

    case SubClassOf(_, subClass, ObjectAllValuesFrom(prop, filler)) =>
      translateAxiom(SubClassOf(prop.getInverseProperty some subClass, filler))

    case SubClassOf(_, subClass, ObjectComplementOf(ce)) =>
      translateAxiom(SubClassOf(ObjectIntersectionOf(subClass, ce), OWLNothing))

    case SubClassOf(_, subClass, ObjectMaxCardinality(max, pe, ce)) => Set.empty //TODO

    case SubClassOf(_, ObjectOneOf(operands), superClass) =>
      operands.flatMap(ind => translateAxiom(ClassAssertion(superClass, ind)))

    case SubClassOf(_, subClass, superClass) if headOkay(superClass) =>
      val level = 0
      val incrementer = new AtomicInteger(level)
      val subject = makeSubject(level)
      translateExpression(superClass, subject, incrementer) match {
        case Intersection(atoms) if atoms.size == 1 =>
          val ruleHead = atoms.head
          translateExpression(subClass, subject, incrementer) match {
            case Union(intersections)           => intersections.map(makeRule(_, ruleHead))
            case intersection @ Intersection(_) => Set(makeRule(intersection, ruleHead))
            case NoAtoms                        => Set.empty
          }
        case _ => Set.empty
      }

    case EquivalentClasses(_, operands) => for {
      superClass <- operands
      subClass <- (operands.filterNot(_ == superClass)) //FIXME scowl types ought to allow minus
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

    case ObjectPropertyRange(_, p, ce)  => translateAxiom(SubClassOf((p.getInverseProperty some OWLThing), ce))

    case SubObjectPropertyOf(_, p, q) =>
      val (x, y) = ("?x", "?y")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} -> ${rel(x, q, y)} ]"))

    case EquivalentObjectProperties(_, operands) => for {
      superProp <- operands
      subProp <- (operands - superProp)
      rule <- translateAxiom(SubObjectPropertyOf(subProp, superProp))
    } yield rule

    case DisjointObjectProperties(_, operands) => (for {
      pair <- operands.toSeq.combinations(2)
      p = pair(0)
      q = pair(1)
    } yield {
      val (x, y) = ("?x", "?y")
      Rule.parseRule(s"[ ${rel(x, p, y)} ${rel(x, q, y)} -> ($x rdf:type owl:Nothing) ($y rdf:type owl:Nothing) ]")
    }).toSet

    case InverseObjectProperties(_, p, q) =>
      val (x, y) = ("?x", "?y")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} -> ${rel(y, q, x)} ]"),
        Rule.parseRule(s"[ ${rel(x, q, y)} -> ${rel(y, p, x)} ]"))

    case FunctionalObjectProperty(_, p) =>
      val (x, y1, y2) = ("?x", "?y1", "?y2")
      Set(Rule.parseRule(s"[ ${rel(x, p, y1)} ${rel(x, p, y2)} -> ($y1 owl:sameAs $y2) ]"))

    case InverseFunctionalObjectProperty(_, p) =>
      val (x1, x2, y) = ("?x", "?x2", "?y")
      Set(Rule.parseRule(s"[ ${rel(x1, p, y)} ${rel(x2, p, y)} -> ($x1 owl:sameAs $x2) ]"))

    case IrreflexiveObjectProperty(_, p) =>
      val x = "?x"
      Set(Rule.parseRule(s"[ ${rel(x, p, x)} -> ($x rdf:type owl:Nothing) ]"))

    case SymmetricObjectProperty(_, p) =>
      val (x, y) = ("?x", "?y")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} -> ${rel(y, p, x)} ]"))

    case AsymmetricObjectProperty(_, p) =>
      val (x, y) = ("?x", "?y")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} ${rel(y, p, x)} -> ($x rdf:type owl:Nothing) ($y rdf:type owl:Nothing) ]"))

    case TransitiveObjectProperty(_, p) =>
      val (x, y, z) = ("?x", "?y", "?z")
      Set(Rule.parseRule(s"[ ${rel(x, p, y)} ${rel(y, p, z)} -> ${rel(x, p, z)} ]"))

    case SubObjectPropertyChainOf(_, subprops, prop) =>
      val start = makeSubject(0)
      val end = makeSubject(subprops.size)
      val patterns = for {
        (subprop, index) <- subprops.zipWithIndex
      } yield rel(makeSubject(index), subprop, makeSubject(index + 1))
      Set(Rule.parseRule(s"[ ${patterns.mkString(" ")} -> ${rel(start, prop, end)} ]"))

    //TODO data properties, abox axioms

    case _ => {
      logger.debug("Skipping: " + axiom)
      Set.empty
    }

  }

  private def headOkay(superClass: OWLClassExpression): Boolean = superClass match {
    case Class(_)                              => true
    case ObjectHasValue(_, NamedIndividual(_)) => true
    case _                                     => false
  }

  private def makeRule(body: Intersection, head: String): Rule = {
    Rule.parseRule(s"[ ${body.atoms.mkString(" ")} -> $head ]")
  }

  private def rel(subj: String, prop: OWLObjectPropertyExpression, obj: String): String = prop match {
    case ObjectProperty(prop)                  => s"($subj <$prop> $obj)"
    case ObjectInverseOf(ObjectProperty(prop)) => s"($obj <$prop> $subj)"
  }

  private def translateExpression(ce: OWLClassExpression, subject: String, incrementer: AtomicInteger): Atoms = ce match {
    case OWLThing   => NoAtoms
    case Class(iri) => Intersection(Set(s"($subject rdf:type <$iri>)"))
    case ObjectSomeValuesFrom(property, filler) =>
      val nextSubject = makeSubject(incrementer.incrementAndGet())
      val triple = Intersection(Set(rel(subject, property, nextSubject)))
      combine(triple, translateExpression(filler, nextSubject, incrementer))
    case ObjectIntersectionOf(operands) => operands.map(translateExpression(_, subject, incrementer))
      .fold(NoAtoms)(combine)
    case ObjectHasValue(property, NamedIndividual(ind)) => Intersection(Set(rel(subject, property, s"<$ind>")))
    case ObjectOneOf(individuals) => Union(for {
      NamedIndividual(ind) <- individuals
    } yield Intersection(Set(s"equal(?x, <$ind>)")))
  }

  private sealed trait Atoms
  private case class Intersection(atoms: Set[String]) extends Atoms
  private case class Union(intersections: Set[Intersection]) extends Atoms
  private case object NoAtoms extends Atoms

  private def combine(a: Atoms, b: Atoms): Atoms = (a, b) match {
    case (i @ Intersection(_), Union(us))   => Union(us.map(intersection => Intersection(i.atoms ++ intersection.atoms)))
    case (Union(us), i @ Intersection(_))   => Union(us.map(intersection => Intersection(i.atoms ++ intersection.atoms)))
    case (Intersection(a), Intersection(b)) => Intersection(a ++ b)
    case (Union(a), Union(b)) => Union(for {
      ai <- a
      bi <- b
    } yield Intersection(ai.atoms ++ bi.atoms))
    case (other, NoAtoms) => other
    case (NoAtoms, other) => other
  }

  private def makeSubject(level: Int): String = if (level == 0) "?x" else s"?x$level"

}