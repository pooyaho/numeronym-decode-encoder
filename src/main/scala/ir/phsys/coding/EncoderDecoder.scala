package ir.phsys.coding

import ir.phsys.util.MathUtil
import ir.phsys.util.SentenceUtils._


/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/13/14
 *         Time: 7:34 PM
 */
object EncoderDecoder {

  import Crawler._

  private lazy val Single = "(.)".r
  private lazy val Tuple = "(.)(.)".r
  private lazy val Multiple = "(.)(.+)(.)".r

  def encode(plain: String): String = {

    def encodeWord(word: String): String = word match {
      case Single(x) =>
        x
      case Tuple(x, y) =>
        x + y
      case Multiple(x, xs, y) =>
        x + xs.size + y
      case "" => ""
    }

    val tokens = plain.tokenizeSentence()
    tokens match {
      case Nil => ""
      case x :: y :: xs =>
        tokens.reduceLeft((a, b) => encodeWord(a) + " " + encodeWord(b)).trim
      case x :: Nil =>
        encodeWord(x)
    }
  }

  def decodeWithHypothesize(encodedText: String, location: Int = -1,
                            useWordIndexAsRank: Boolean = false): List[(String, Double)] = {

    assert(encodedText.length >= 2)
    if (!getIndexedWordMap.keys.toList.contains(location) && location >= 0) {
      throw new IllegalArgumentException("Location not found!")
    }
    val regex = encodedText.length match {
      case 2 => encodedText
      case _ =>
        val count = encodedText.charAt(1)
        encodedText.replace(s"$count", s"(.{$count})")
    }

    val sortedMap = if (location < 0) {
      if (!useWordIndexAsRank)
        indexedWordMap.map {
          case (k, v) => (k, v.filter(_.toString.matches(regex)))
        }.values.flatten.groupBy(identity).mapValues(_.size.toDouble).toList
      else {
        val series = MathUtil.generatePriorityProbabilitySeries(Crawler.getIndexedWordMap.size)
        indexedWordMap.map {
          case (k, v) => (k, v.filter(_.toString.matches(regex)))
        }.map {
          case (k, v) => (k, v.groupBy(identity).mapValues(_.size * series(k)).toList)
        }.values.flatten.groupBy(_._1).mapValues(_.map(_._2).sum).toList
      }
    } else {
      indexedWordMap(location).filter(_.toString.matches(regex)).groupBy(identity)
      .mapValues(_.size.toDouble)
      .toList
    }
    val allWordsCount = sortedMap.map(_._2).sum
    //    val allWordsCount = 10.0

    sortedMap.map(tuple => (tuple._1.toString, tuple._2.toDouble / allWordsCount)).sortWith(_._2 > _._2)
    //    sortedMap.map{
    //      case (x,y)=>(x.toString,y.asInstanceOf[Double])
    //    }
  }

  //  def decodeWithHypothesize(encodedText: String): List[(String, Float)] = {
  //    assert(encodedText.length >= 2)
  //
  //    val regex = encodedText.length match {
  //      case 2 => encodedText
  //      case _ =>
  //        val count = encodedText.charAt(1)
  //        encodedText.replace(s"$count", s"(.{$count})")
  //    }
  //
  //    val unsortedMap = indexedWordMap.map {
  //      case (k, v) => (k, v.filter(_.toString.matches(regex)).groupBy(identity).mapValues(_.size).toList)
  //    }.values.flatten.toList
  //
  //    val allWordsCount = unsortedMap.map(_._2).sum
  //
  //
  //    unsortedMap.map(p => (p._1.toString, p._2.asInstanceOf[Float] / allWordsCount)).sortWith(_._2 > _._2)
  //  }

  def decodeWord(encodedText: String, location: Int): String = {
    assert(encodedText.tokenizeSentence().size <= 1)
    decodeWithHypothesize(encodedText, location).head._1
  }

  def decodeWord(encodedText: String): String = {
    decodeWithHypothesize(encodedText).head._1
  }

  def decodeSentence(sentence: String): String = {

    val splitted = sentence.tokenizeSentence().zipWithIndex
    assert(splitted.size >= 1)
    //    splitted.reduceLeft((a, b) => decodeWord(a._1, a._2) +""+ decodeWord(b._1, b._2))
    ""
  }
}
