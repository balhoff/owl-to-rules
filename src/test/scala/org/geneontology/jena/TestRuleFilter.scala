package org.geneontology.jena

import org.apache.jena.reasoner.rulesys.Rule
import org.phenoscape.scowl._

class TestRuleFilter extends UnitSpec {

  "owl:Nothing" should "not generate rules as a subclass" in {

    val PartOf = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
    val NucleusOfBrain = Class("http://purl.obolibrary.org/obo/UBERON_0002308")
    val Midbrain = Class("http://purl.obolibrary.org/obo/UBERON_0001891")
    val axiom = OWLNothing EquivalentTo (NucleusOfBrain and (PartOf some Midbrain))
    val rules = OWLtoRules.translateAxiom(axiom)
    rules.size shouldEqual 1
    rules.head shouldEqual Rule.parseRule("[ (?x1 rdf:type <http://purl.obolibrary.org/obo/UBERON_0002308>) (?x1 <http://purl.obolibrary.org/obo/BFO_0000050> ?x2) (?x2 rdf:type <http://purl.obolibrary.org/obo/UBERON_0001891>) -> (?x1 rdf:type owl:Nothing) ]")
  }

}