package org.geneontology.jena

import scala.collection.JavaConversions._

import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.Statement
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner
import org.apache.jena.vocabulary.OWL2
import org.apache.jena.vocabulary.RDF
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.InferredClassAssertionAxiomGenerator
import org.semanticweb.owlapi.util.InferredOntologyGenerator
import org.semanticweb.owlapi.util.InferredPropertyAssertionGenerator

import uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory
import org.apache.jena.rdf.model.ResourceFactory
import java.io.PrintWriter
import org.apache.jena.reasoner.rulesys.RuleDerivation
import org.semanticweb.owlapi.util.InferredIndividualAxiomGenerator
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory
import org.semanticweb.HermiT.ReasonerFactory

class TestRules extends UnitSpec {

  private val TopObjectProperty = OWL2.topObjectProperty.getURI
  private val RDFType = RDF.`type`.getURI
  private val OWLThing = OWL2.Thing.getURI

  "Explanations" should "be obtainable" in {
    // This doesn't really test anything, it's just running the explanation function
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("ro-merged.owl"))
    val rules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true, true)
    val reasoner = new GenericRuleReasoner(rules.toList)
    reasoner.setMode(GenericRuleReasoner.FORWARD_RETE)
    reasoner.setDerivationLogging(true)
    val dataModel = ModelFactory.createDefaultModel()
    dataModel.read(this.getClass.getResourceAsStream("57c82fad00000639.ttl"), "", "ttl")
    val infModel = ModelFactory.createInfModel(reasoner, dataModel)
    infModel.prepare()

    Explanation.explain(ResourceFactory.createStatement(
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000649"),
      ResourceFactory.createProperty("http://purl.obolibrary.org/obo/BFO_0000051"),
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000653")), infModel)
      .foreach(e => println(e.toString))

    Explanation.explain(ResourceFactory.createStatement(
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000653"),
      ResourceFactory.createProperty("http://purl.obolibrary.org/obo/RO_0002410"),
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000656")), infModel)
      .foreach(e => println(e.toString))

    Explanation.explain(ResourceFactory.createStatement(
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000653"),
      ResourceFactory.createProperty("http://purl.obolibrary.org/obo/RO_0002608"),
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000656")), infModel)
      .foreach(e => println(e.toString))

    Explanation.explain(ResourceFactory.createStatement(
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000656"),
      ResourceFactory.createProperty("http://purl.obolibrary.org/obo/RO_0002500"),
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000653")), infModel)
      .foreach(e => println(e.toString))

    Explanation.explain(ResourceFactory.createStatement(
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000655"),
      ResourceFactory.createProperty("http://purl.obolibrary.org/obo/BFO_0000050"),
      ResourceFactory.createResource("http://model.geneontology.org/57c82fad00000639/57c82fad00000653")), infModel)
      .foreach(e => println(e.toString))
  }

  "Jena rules" should "infer the same triples as an OWL API reasoner" in {
    compare("57c82fad00000639.ttl", "ro-merged.owl", false)
    compare("test1.ttl", "test1.ttl", true)
  }

  def compare(dataFile: String, ontologyFile: String, hermit: Boolean): Unit = {
    val asserted = {
      val model = ModelFactory.createDefaultModel()
      model.read(this.getClass.getResourceAsStream(dataFile), "", "ttl")
      filterStatements(model.listStatements().toSet.toSet)
    }
    val jena = runJenaRules(dataFile, ontologyFile)
    val owlResult = if (hermit) runHermiT(dataFile, ontologyFile) else runFactPlusPlus(dataFile, ontologyFile)
    jena.size should be > asserted.size
    owlResult.size should be > asserted.size
    println("Not in jena: ")
    (owlResult -- jena).foreach(println)
    println("Not in OWL reasoner: ")
    (jena -- owlResult).foreach(println)
    jena shouldEqual owlResult
  }

  def runJenaRules(dataFile: String, ontologyFile: String): Set[Statement] = {
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(ontologyFile))
    val rules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true, true)
    rules.foreach(println)
    val reasoner = new GenericRuleReasoner(rules.toList)
    reasoner.setMode(GenericRuleReasoner.FORWARD_RETE)
    val dataModel = ModelFactory.createDefaultModel()
    dataModel.read(this.getClass.getResourceAsStream(dataFile), "", "ttl")
    val infModel = ModelFactory.createInfModel(reasoner, dataModel)
    filterStatements(infModel.listStatements().toSet.toSet)
  }

  def runOWLReasoner(factory: OWLReasonerFactory, dataFile: String, ontologyFile: String): Set[Statement] = {
    val manager = OWLManager.createOWLOntologyManager()
    if (ontologyFile != dataFile) manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(ontologyFile))
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(dataFile))
    val reasoner = factory.createReasoner(ontology)
    val generator = new InferredOntologyGenerator(reasoner, List(
      new InferredPropertyAssertionGenerator(),
      new InferredClassAssertionAxiomGenerator()))
    generator.fillOntology(manager.getOWLDataFactory, ontology)
    reasoner.dispose()
    filterStatements(SesameJena.ontologyAsTriples(ontology))
  }

  def runFactPlusPlus(dataFile: String, ontologyFile: String): Set[Statement] =
    runOWLReasoner(new FaCTPlusPlusReasonerFactory(), dataFile, ontologyFile)

  def runHermiT(dataFile: String, ontologyFile: String): Set[Statement] =
    runOWLReasoner(new ReasonerFactory(), dataFile, ontologyFile)

  def filterStatements(statements: Set[Statement]): Set[Statement] =
    statements.filterNot(_.getPredicate.getURI == TopObjectProperty)
      .filterNot(_.getPredicate.getURI == OWL2.sameAs.getURI)
      .filterNot(_.getPredicate.getURI == OWL2.differentFrom.getURI)
      .filterNot(_.getSubject.isAnon)
      .filterNot(_.getObject.isAnon)
      .filterNot(s => (s.getPredicate.getURI == RDFType) && s.getObject.isURIResource && (s.getObject.asResource.getURI == OWLThing))

}