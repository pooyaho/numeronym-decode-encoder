package ir.phsys.coding

import scala.beans.BeanProperty
import scala.collection.mutable
import ir.phsys.util.SentenceUtils._
import scala.tools.nsc.io.Directory
import scala.reflect.io.File


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

  def generateIndexedWordMapFromFile(file: File): Unit = {
    import scala.io.Source

    val strbuilder = new StringBuilder

    Source.fromInputStream(file.inputStream()).getLines().foreach {
      case x => strbuilder.append(x)
    }

    strbuilder.toString().tokenizeText().foreach {
      case x => generateIndexedWordMap(x.trim)
    }
  }

  def generateIndexedWordMapFromDir(dir: Directory): Unit = {
    (dir deepFiles) foreach generateIndexedWordMapFromFile
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