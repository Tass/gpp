package gpp

import nak.core._
import nak.data._
import nak.NakContext._
import nak.util.ConfusionMatrix

trait Model extends (Example[String, String] => String) {
}

trait Method {
  def apply(featurizer: Featurizer[String, String], train: Seq[Example[String, String]], cost: Double): Model
}

trait Feature {
  def apply(parsed: List[util.Token]): Iterable[String]
}

object Opts {
  import org.rogach.scallop._
  val methods = Map[String, Method](
    "majority" -> Majority,
    "lexicon" -> Lexicon,
    "L2R_LR" -> Supervised
  )
  val feats = Map[String, Feature](
    "tokens" -> Tokens,
    "tags" -> Tags,
    "tokensntags" -> TokensnTags,
    "tagbigrams" -> TagBigrams,
    "tokenbigrams" -> TokenBigrams,
    "MPQA" -> MPQASimple,
    "MPQAcomplex" -> MPQAComplex,
    "numbers" -> Numbers
  )
  val version = "0.0.0"

  def apply(args: Array[String]) = new ScallopConf(args) {
    val version = Opts.version
    banner(s"""
Classification application version $version

For usage see below:
	     """)
    // --help is baked into scallop, can't use short.
    val verbose = opt[Boolean]()
    val detailed = opt[Boolean]()
    val extended = opt[Boolean](short='x', descr="Use extended features. A shortcut for selecting all features. -f is ignored.")
    val eval = opt[String](required = true, descr = "The files containing evalualation events.")
    val method = opt[String](descr = "The type of solver to use. Possible values: " + methods.keySet.mkString(", "), validate = methods.keySet, required = true)
    val train = opt[List[String]](required = true, descr = "The files containing training events.")
    val cost = opt[Double](descr = "The cost parameter C. Bigger values means less regularization (more fidelity to the training set). (default = 1.0)", default = Some(1.0))
    val fts = opt[List[String]]("features", descr = "The features to use. Combine as needed: " + feats.keySet.mkString(", "), validate = (list => list.forall(feats.keySet)))
    // Somehow defaults for List[A] don't work as expected
    lazy val features = if (fts().size == 0) List("tokens") else fts()
  }
}

object Exp {
  def main(args: Array[String]) {
    val opts = Opts(args)
    val method = Opts.methods(opts.method())
    val trainEx = opts.train().map(parseXML(_))
    val featurizer = if(opts.extended()) new FeatureCollector(Opts.feats.values.toList) else new FeatureCollector(opts.features.map(Opts.feats))
    val model = method(featurizer, trainEx.flatten, opts.cost())
    val evalEx = parseXML(opts.eval())
    val results = evalEx.map(model)
    val matrix = ConfusionMatrix(evalEx.map(_.label), results, evalEx.map(_.features))
    println(if (opts.detailed()) matrix.detailedOutput else matrix)
  }

  import com.codecommit.antixml._
  def parseXML(filename: String): Seq[Example[String, String]] = {
    val text: Selector[String] = Selector({case Text(str) => str})
    val nodes: Group[Elem] = XML.fromInputStream(new java.io.FileInputStream(filename)) \ "item"
    nodes.map(node => Example(node.attr("label").get, (node \ "content" \ text).mkString))
  }
}

object ConvertStanford {
  def main(args: Array[String]) {
    val featuremap = Map("0" -> "negative", "2" -> "neutral", "4" -> "positive")
    println(io.Source.fromFile(args(0)).getLines.map(_.split(";;")).map({list =>
      <item label={featuremap(list(0))}>
        <content>{list(5)}</content>
      </item>
    }).mkString("<dataset>", "\n", "</dataset>"))
  }
}

object ConvertEmoticon {
  import java.io.File
  def main(args: Array[String]) {
    val path = new File(args(0))
    println(Iterable(("happy.txt" -> "positive"), ("neutral.txt" -> "neutral"), ("sad.txt" -> "negative")).map({
      case (file, label) =>
        io.Source.fromFile(new File(path, file)).getLines.map(tweet =>
          <item label={label}>
            <content>{tweet}</content>
          </item>
          ).mkString("\n")
    }).mkString("<dataset>", "\n", "</dataset>"))
  }
}
