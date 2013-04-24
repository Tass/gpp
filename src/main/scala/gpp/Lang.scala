package gpp.util
import java.util.zip.GZIPInputStream

trait Lexicon extends Function1[Token, Option[Double]]

object MPQA extends Lexicon {
  lazy val file = io.Source.fromInputStream(new GZIPInputStream(Thread.currentThread.getContextClassLoader.getResourceAsStream("subjclueslen1-HLTEMNLP05.tff.gz"))).getLines.filterNot(_.startsWith("#"))
  lazy val lexicon = file.map(_.split(" ").map(_.split("=")(1))).foldLeft(Map[String, Map[String, Double]]())({
    case (map, list) =>
      val word = list(2)
      val value = map.getOrElse(word, Map()) + (list(3) -> 
                                                (if (list(0) == "strongsubj") 1 else 0.5) * (if (list(5) == "positive") 1 else -1))
      map + (word -> value)
  })
  def apply(tk: Token): Option[Double] = {
    val Token(token, tag) = tk
    lexicon.get(token.toLowerCase).map(tm => tm.get(tagmap(tag)).orElse(tm.get("anypos")).orElse(tm.headOption.map(_._2)))}.flatten

  val tagmap = Map("a" -> "adj", "v" -> "verb", "n" -> "noun").withDefault({case _ =>"anypos"})
}

// object Liu extends Lexicon {
// }
