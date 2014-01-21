package ir.phsys.coding

import scala.beans.BeanProperty
import scala.collection.mutable
import ir.phsys.util.SentenceUtils._


/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/13/14
 *         Time: 9:21 PM
 */
object Crawler {
  @BeanProperty
  val indexedWordMap = mutable.Map[Int, List[Any]]()

  def generateIndexedWordMap(str: String): Unit = {
    str.tokenizeSentence().zipWithIndex.foreach {
      case (v, i) => indexedWordMap(i) = List(v) ++ indexedWordMap.getOrElse(i, List(""))
    }
  }

  def generateIndexedWordMapFromFile(path: String): Unit = {
    import scala.io.Source
    val strbuilder = new StringBuilder

    Source.fromFile(path).getLines().foreach {
      case x => strbuilder.append(x)
    }

    strbuilder.toString().tokenizeText().foreach {
      case x => generateIndexedWordMap(x.trim)
    }
  }

  def generateIndexedWordMapFromWikipedia(url: String): Unit = {
    import scala.io.Source
    val strbuilder = new StringBuilder

    Source.fromURL(url).getLines().foreach {
      case x => strbuilder.append(x)
    }

    strbuilder.toString().tokenizeSentence().foreach {
      case x => generateIndexedWordMap(x.trim)
    }
  }
}