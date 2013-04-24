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

object Opts {
  import org.rogach.scallop._
  val methods = Map[String, Method](
    "majority" -> Majority,
    "lexicon" -> Lexicon
  )
  val version = "0.0.0"

  def apply(args: Array[String]) = new ScallopConf(args) {
    val version = Opts.version
    banner(s"""
Classification application version $version

For usage see below:
	     """)
    // --help is baked into scallop, can't use short.
    val verbose = opt[Boolean]("verbose")
    val detailed = opt[Boolean]("detailed")
    val extended = opt[Boolean]("extended", short='x', descr="Use extended features.")
    val eval = opt[String]("eval", required = true, descr = "The files containing evalualation events.")
    val method = opt[String]("method", descr = "The type of solver to use. Possible values: " + methods.keySet.mkString(", "), validate = methods.keySet, required = true)
    val train = opt[String]("train", required = true, descr = "The files containing training events.")
    val cost = opt[Double]("cost", descr = "The cost parameter C. Bigger values means less regularization (more fidelity to the training set). (default = 1.0)", default = Some(1.0))
  }
}

object Exp {
  def main(args: Array[String]) {
    val opts = Opts(args)
    val method = Opts.methods(opts.method())
    val trainEx = parseXML(opts.train())
    val model = method(new Featurizer[String, String] {
      def apply(input: String) = List()
    }, trainEx, opts.cost())
    val evalEx = parseXML(opts.eval())
    val results = evalEx.map(model)
    println(ConfusionMatrix(evalEx.map(_.label), results, evalEx.map(_.features)))
  }

  import com.codecommit.antixml._
  def parseXML(filename: String): Seq[Example[String, String]] = {
    val text: Selector[String] = Selector({case Text(str) => str})
    val nodes: Group[Elem] = XML.fromSource(io.Source.fromFile(filename)) \ "item"
    nodes.map(node => Example(node.attr("label").get, (node \ "content" \ text).mkString))
  }
}
