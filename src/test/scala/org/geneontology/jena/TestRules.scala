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

class TestRules extends UnitSpec {

  private val TopObjectProperty = OWL2.topObjectProperty.getURI
  private val RDFType = RDF.`type`.getURI
  private val OWLThing = OWL2.Thing.getURI

  "Jena rules" should "infer the same triples as FaCT++" in {
    compare("57c82fad00000639.ttl", "ro-merged.owl")
    compare("test1.ttl", "test1.ttl")
  }

  def compare(dataFile: String, ontologyFile: String): Unit = {
    val asserted = {
      val model = ModelFactory.createDefaultModel()
      model.read(this.getClass.getResourceAsStream(dataFile), "", "ttl")
      filterStatements(model.listStatements().toSet.toSet)
    }
    val jena = runJenaRules(dataFile, ontologyFile)
    val fact = runFactPlusPlus(dataFile, ontologyFile)
    jena.size should be > asserted.size
    fact.size should be > asserted.size
    println("Not in jena: ")
    (fact -- jena).foreach(println)
    println("Not in fact: ")
    (jena -- fact).foreach(println)
    jena shouldEqual fact
  }

  def runJenaRules(dataFile: String, ontologyFile: String): Set[Statement] = {
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(ontologyFile))
    val rules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true)
    val reasoner = new GenericRuleReasoner(rules.toList)
    val dataModel = ModelFactory.createDefaultModel()
    dataModel.read(this.getClass.getResourceAsStream(dataFile), "", "ttl")
    val infModel = ModelFactory.createInfModel(reasoner, dataModel)
    filterStatements(infModel.listStatements().toSet.toSet)
  }

  def runFactPlusPlus(dataFile: String, ontologyFile: String): Set[Statement] = {
    val manager = OWLManager.createOWLOntologyManager()
    if (ontologyFile != dataFile) manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(ontologyFile))
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(dataFile))
    val reasoner = new FaCTPlusPlusReasonerFactory().createReasoner(ontology)
    val generator = new InferredOntologyGenerator(reasoner, List(new InferredPropertyAssertionGenerator(), new InferredClassAssertionAxiomGenerator()))
    generator.fillOntology(manager.getOWLDataFactory, ontology)
    reasoner.dispose()
    filterStatements(SesameJena.ontologyAsTriples(ontology))
  }

  def filterStatements(statements: Set[Statement]): Set[Statement] =
    statements.filterNot(_.getPredicate.getURI == TopObjectProperty)
      .filterNot(_.getSubject.isAnon)
      .filterNot(_.getObject.isAnon)
      .filterNot(s => (s.getPredicate.getURI == RDFType) && s.getObject.isURIResource && (s.getObject.asResource.getURI == OWLThing))

}