package gpp
import nak.core._
import nak.data._
import nak.NakContext._
import org.reactormonk.Counter

class Majority(label: String) extends Model {
  def apply(ex: Example[String, String]) = label
}

object Majority extends Method {
  def apply(featurizer: Featurizer[String, String], train: Seq[Example[String, String]], cost: Double): Model = {
    val label = Counter(train.map(_.label)).max
    new Majority(label)
  }
}
