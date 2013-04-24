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

object SimpleFeatures extends Featurizer[String, String]{
  def apply(raw: String) = {
    val parsed = util.POSTagger(raw)
    val features = scala.collection.mutable.ListBuffer[String]()
    features.appendAll(parsed.flatMap(token => List("token=" + token.token.toLowerCase, "tag=" + token.tag, "token+tag=" + token.token + "+" + token.tag)))
    features.appendAll(parsed.map(util.MPQA(_).map(_.toString)).flatten)
    features.map(FeatureObservation(_))
  }
}
