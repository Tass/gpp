package gpp

import nak.core._
import nak.data._
import nak.NakContext._
import nak.liblinear.LiblinearConfig

class Supervised(classifier: FeaturizedClassifier[String, String], maxLabelPpa: Function1[Seq[Double], String]) extends Model {
  def apply(ex: Example[String, String]) = maxLabelPpa(classifier.evalRaw(ex.features))
}

object Supervised extends Method {
  def apply(featurizer: Featurizer[String, String], train: Seq[Example[String, String]], cost: Double): Model = {
    val config = LiblinearConfig(cost=cost)
    val classifier = trainClassifier(config, featurizer, train)
    def maxLabelPpa = maxLabel(classifier.labels) _
    new Supervised(classifier, maxLabelPpa)
  }
}

class FeatureCollector(feats: List[Feature]) extends Featurizer[String, String]{
  def apply(raw: String) = {
    val parsed = util.POSTagger(raw)
    feats.flatMap(_.apply(parsed))
  }
}
object Tokens extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.map(token => "token=" + token.token.toLowerCase).map(FeatureObservation(_))
}

object Tags extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.map(token => "tag=" + token.tag).map(FeatureObservation(_))
}

object TokensnTags extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.map(token => "token+tag=" + token.token.toLowerCase + "+" + token.tag).map(FeatureObservation(_))
}

object TagBigrams extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.sliding(2).map(list => list.map(_.tag).mkString("bigramTags=", "+", "")).map(FeatureObservation(_)).toList
}

object TokenBigrams extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.sliding(2).map(list => list.map(_.token.toLowerCase).mkString("bigramTokens=", "+", "")).map(FeatureObservation(_)).toList
}

object MPQALex extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.map(util.MPQA(_).map(_.toString)).flatten.map(FeatureObservation(_)).toList
}

object Numbers extends Feature {
  def apply(parsed: List[util.Token]): List[FeatureObservation[String]] = parsed.map(token => """([+-])\d""".r.findFirstIn(token.token).map(_(0).toString)).flatten.map(FeatureObservation(_)).toList
}
