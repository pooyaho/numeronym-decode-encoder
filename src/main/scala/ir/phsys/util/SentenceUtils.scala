package ir.phsys.util

import java.text.BreakIterator


/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/20/14
 *         Time: 7:25 PM
 */
object SentenceUtils {

  implicit class Tokenizer(text: String) {

    def tokenizeSentence(): List[String] = {
      //      val pattern = "(\\ )+|(,)|(\n)|(\t)"
      val pattern = "\\s+|(,)(\\s*)"
      text.trim.split(pattern).toList
    }

    def tokenizeText(): List[String] = {
      //      text.split("((\\.+)((\\ +)|(\n+)))|(\n)").toList
      val op = BreakIterator.getSentenceInstance
      op.setText(text)
      var start = op.first()

      Stream.continually(op.next()).takeWhile(_ != BreakIterator.DONE).map {
        case x =>
          val st = start
          start = x
          text.substring(st, x) match {
            case sentence if sentence.endsWith(".") => sentence.substring(0, sentence.length - 1)
            case other => other
          }
      }.toList
    }

  }

}