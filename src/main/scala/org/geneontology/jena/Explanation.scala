package org.geneontology.jena

import scala.collection.JavaConverters._

import org.apache.jena.rdf.model.InfModel
import org.apache.jena.rdf.model.Statement
import org.apache.jena.reasoner.rulesys.Rule
import org.apache.jena.reasoner.rulesys.RuleDerivation

object Explanation {

  def explain(statement: Statement, model: InfModel): Set[Explanation] = explain(Set(statement), model)

  def explain(statements: Set[Statement], model: InfModel): Set[Explanation] = {
    val subExplanations = for {
      statement <- statements
    } yield {
      val derivations = model.getDerivation(statement).asScala.toSet
      if (derivations.isEmpty) Set(Explanation(Set(statement), Set.empty))
      else {
        for {
          derivation <- derivations.collect { case d: RuleDerivation => d }
          current = Explanation(Set.empty, Set(derivation.getRule))
          facts = derivation.getMatches.asScala.map(model.asStatement(_)).toSet
          subExplanation <- explain(facts, model).map(e => combine(e, current))
        } yield {
          subExplanation
        }
      }
    }
    cartesianProduct(subExplanations).map(_.reduce(combine))
  }

  def cartesianProduct[T](xss: Set[Set[T]]): Set[Set[T]] = xss match {
    case e if e.isEmpty => Set(Set.empty)
    case f =>
      val head = f.head
      val tail = f - head
      for {
        xh <- head
        xt <- cartesianProduct(tail)
      } yield xt + xh
  }

  private def combine(a: Explanation, b: Explanation): Explanation = Explanation(a.facts ++ b.facts, a.rules ++ b.rules)

}

final case class Explanation(facts: Set[Statement], rules: Set[Rule]) {

  override def toString: String = "Facts:\n" + facts.mkString("\n") + "\nRules:\n" + rules.mkString("\n")

}