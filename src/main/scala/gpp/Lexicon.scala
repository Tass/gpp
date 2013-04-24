package gpp
import nak.core._
import nak.data._
import nak.NakContext._

class Lexicon() extends Model {
  val threshold = 0.2
  def apply(ex: Example[String, String]): String = {
    val tagged = gpp.util.POSTagger(ex.features)
    val values = tagged.flatMap(gpp.util.MPQA.apply)
    values.sum/values.size match {
      case _ if values.size == 0 => "undecided"
      case x if x < -threshold => "negative"
      case x if x > threshold => "positive"
      case x => "neutral"
    }
  }
}

object Lexicon extends Method {
  def apply(featurizer: Featurizer[String, String], train: Seq[Example[String, String]], cost: Double): Model = {
    new Lexicon()
  }
}
