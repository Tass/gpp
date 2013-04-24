package gpp
import nak.core._
import nak.data._
import nak.NakContext._
import java.util.zip.GZIPInputStream;

class Lexicon(lexicon: Map[String, Map[String, Double]]) extends Model {
  val tagmap = Map("a" -> "adj", "v" -> "verb", "n" -> "noun").withDefault({case _ =>"anypos"})
  val threshold = 0.2
  def apply(ex: Example[String, String]): String = {
    val tagged = POSTagger(ex.features)
    val values = tagged.flatMap({case Token(token, tag) => lexicon.get(token.toLowerCase).map(tm => tm.get(tag).orElse(tm.get("anypos")).orElse(tm.headOption.map(_._2)))}).flatten
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
    val file = io.Source.fromInputStream(new GZIPInputStream(Thread.currentThread.getContextClassLoader.getResourceAsStream("subjclueslen1-HLTEMNLP05.tff.gz"))).getLines.filterNot(_.startsWith("#"))
    val lexicon = file.map(_.split(" ").map(_.split("=")(1))).foldLeft(Map[String, Map[String, Double]]())({
      case (map, list) =>
        val word = list(2)
        val value = map.getOrElse(word, Map()) + (list(3) -> 
          (if (list(0) == "strongsubj") 1 else 0.5) * (if (list(5) == "positive") 1 else -1))
        map + (word -> value)
      })
    new Lexicon(lexicon)
  }
}
