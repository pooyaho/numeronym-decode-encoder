package ir.phsys.util

/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/20/14
 *         Time: 7:25 PM
 */
object SentenceUtils {

  implicit class Tokenizer(text: String) {
    def tokenizeSentence(): List[String] = {
      val pattern="(\\ )|(\\.)"
      text.split(pattern).toList
    }

    def tokenizeText(): List[String] = {
      text.split("\\. ").toList
    }
  }

}
