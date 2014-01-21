package ir.phsys.test

import org.scalatest.FunSuite
import ir.phsys.coding.{EncoderDecoder, Crawler}
import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.struct.DirectedGraphEdgeIterator
import ir.phsys.util.SentenceUtils._

/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/13/14
 *         Time: 9:24 PM
 */
class TestScala extends FunSuite {
  //  val default=mutable.Buffer.empty[String]
  test("Indexize the string") {
    Crawler.generateIndexedWordMapFromFile("/home/pooya/Desktop/txt")
    val map = Crawler.getIndexedWordMap
    (0 to map.keys.max).foreach {
      case x => println(s"index is :$x and value is ${map(x)}")
    }

    val code = EncoderDecoder.encode("Spanag")
    assert(code.equals("S4g"))
    println(EncoderDecoder.decodeWithHypothesize(code, useWordIndexAsRank = true))
    assert(EncoderDecoder.decodeWord(code) == "Spring")

    val sentenceCoded = EncoderDecoder.encode("Spring is the best")
    println(sentenceCoded)
    //    val regex = code.length match {
    //      case 2 => code
    //      case _ =>
    //        val count = code.charAt(1)
    //        code.replace(s"$count", s"(.{$count})")
    //    }
    //    val series = MathUtil.generatePriorityProbabilitySeries(Crawler.getIndexedWordMap.keys.max)
    //
    //    val indexedmap = Crawler.getIndexedWordMap.map {
    //      case (k, v) => (k, v.filter(_.toString.matches(regex)))
    //    }.map {
    //                       case (k, v) => (k, v.groupBy(identity).mapValues(_.size * series(k)).toList)
    //                     }.values.flatten.groupBy(_._1).mapValues(_.map(_._2).sum).toList.sortWith(_._2 > _._2)
    //    println(series)
    //    println(indexedmap)
  }

  test("Test xml") {
    val a = "<div class=\"dablink\">This article is about the corporation.&#32;&#32;For the search engine, see <a href=\"/wiki/Google_Search\" title=\"Google Search\">Google Search</a>.&#32;&#32;For other uses, see <a href=\"/wiki/Google_(disambiguation)\" title=\"Google (disambiguation)\">Google (disambiguation)</a>.</div>\n<div class=\"dablink\">Not to be confused with <a href=\"/wiki/Googol\" title=\"Googol\">Googol</a>.</div>\n<p>\n</p>\n<div class=\"metadata topicon nopopups\" id=\"protected-icon\" style=\"display:none; right:55px;\"><a href=\"/wiki/Wikipedia:Protection_policy#semi\" title=\"This article is semi-protected indefinitely in response to an ongoing high risk of vandalism.\"><img alt=\"Page semi-protected\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Padlock-silver.svg/20px-Padlock-silver.svg.png\" width=\"20\" height=\"20\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Padlock-silver.svg/30px-Padlock-silver.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Padlock-silver.svg/40px-Padlock-silver.svg.png 2x\" /></a></div>\n<table class=\"infobox vcard\" cellspacing=\"3\" style=\"border-spacing:3px;width:22em;\"><caption class=\"fn org\">Google Inc.</caption><tr><td colspan=\"2\" class=\"logo\" style=\"text-align:center;\">\n<a href=\"/wiki/File:Logo_Google_2013_Official.svg\" class=\"image\"><img alt=\"Google Logo\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Logo_Google_2013_Official.svg/220px-Logo_Google_2013_Official.svg.png\" width=\"220\" height=\"78\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Logo_Google_2013_Official.svg/330px-Logo_Google_2013_Official.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Logo_Google_2013_Official.svg/440px-Logo_Google_2013_Official.svg.png 2x\" /></a></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Types_of_business_entity\" title=\"Types of business entity\">Type</a></th><td class=\"category\">\nPublic</td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Ticker_symbol\" title=\"Ticker symbol\">Traded as</a></th><td>\n<a href=\"/wiki/NASDAQ\" title=\"NASDAQ\">NASDAQ</a>:&#160;<a rel=\"nofollow\" class=\"external text\" href=\"http://www.nasdaq.com/symbol/goog\">GOOG</a><br><a href=\"/wiki/NASDAQ-100\" title=\"NASDAQ-100\">NASDAQ-100 Component</a><br><a href=\"/wiki/S%26P_500\" title=\"S&amp;P 500\">S&amp;P 500 Component</a></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Industry</th><td class=\"category\">\n<a href=\"/wiki/Internet\" title=\"Internet\">Internet</a><br>Computer <a href=\"/wiki/Software\" title=\"Software\">software</a><br><a href=\"/wiki/Telecoms_equipment\" title=\"Telecoms equipment\" class=\"mw-redirect\">Telecoms equipment</a></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Founded</th><td>\n<a href=\"/wiki/Menlo_Park,_California\" title=\"Menlo Park, California\">Menlo Park, California</a><br>(September&#160;4,&#160;1998<span style=\"display:none\">&#160;(<span class=\"bday dtstart published updated\">1998-09-04</span>)</span>)<sup id=\"cite_ref-1\" class=\"reference\"><a href=\"#cite_note-1\"><span>[</span>1<span>]</span></a></sup><sup id=\"cite_ref-2\" class=\"reference\"><a href=\"#cite_note-2\"><span>[</span>2<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Founder(s)</th><td class=\"agent\">\nLarry Page, Sergey Brin</td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Headquarters</th><td class=\"adr\">\n<span class=\"locality\">Googleplex, <a href=\"/wiki/Mountain_View,_California\" title=\"Mountain View, California\">Mountain View, California</a>, U.S.<sup id=\"cite_ref-3\" class=\"reference\"><a href=\"#cite_note-3\"><span>[</span>3<span>]</span></a></sup></span></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Area served</th><td>\nWorldwide</td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Key people</th><td class=\"agent\">\n<a href=\"/wiki/Eric_Schmidt\" title=\"Eric Schmidt\">Eric Schmidt</a><br>(<a href=\"/wiki/Chairman\" title=\"Chairman\">Executive Chairman</a>)<br><a href=\"/wiki/Larry_Page\" title=\"Larry Page\">Larry Page</a><br>(Co-founder &amp; <a href=\"/wiki/CEO\" title=\"CEO\" class=\"mw-redirect\">CEO</a>)<br><a href=\"/wiki/Sergey_Brin\" title=\"Sergey Brin\">Sergey Brin</a> (Co-founder)</td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Products</th><td>\n<i>See</i> <a href=\"/wiki/List_of_Google_products\" title=\"List of Google products\">list of Google products</a></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Revenue</th><td>\n<span title=\"Increase\"><img alt=\"Increase\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/11px-Increase2.svg.png\" width=\"11\" height=\"11\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/17px-Increase2.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/22px-Increase2.svg.png 2x\" /></span> US$ 50.18&#160;billion (2012)<sup id=\"cite_ref-Google-Inc-Jan-2013-10-K_4-0\" class=\"reference\"><a href=\"#cite_note-Google-Inc-Jan-2013-10-K-4\"><span>[</span>4<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Earnings_before_interest_and_taxes\" title=\"Earnings before interest and taxes\">Operating income</a></th><td>\n<span title=\"Increase\"><img alt=\"Increase\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/11px-Increase2.svg.png\" width=\"11\" height=\"11\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/17px-Increase2.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/22px-Increase2.svg.png 2x\" /></span> US$ 12.76&#160;billion (2012)<sup id=\"cite_ref-Google-Inc-Jan-2013-10-K_4-1\" class=\"reference\"><a href=\"#cite_note-Google-Inc-Jan-2013-10-K-4\"><span>[</span>4<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Net_income\" title=\"Net income\">Profit</a></th><td>\n<span title=\"Increase\"><img alt=\"Increase\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/11px-Increase2.svg.png\" width=\"11\" height=\"11\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/17px-Increase2.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/22px-Increase2.svg.png 2x\" /></span> US$ 10.74&#160;billion (2012)<sup id=\"cite_ref-Google-Inc-Jan-2013-10-K_4-2\" class=\"reference\"><a href=\"#cite_note-Google-Inc-Jan-2013-10-K-4\"><span>[</span>4<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Asset\" title=\"Asset\">Total assets</a></th><td>\n<span title=\"Increase\"><img alt=\"Increase\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/11px-Increase2.svg.png\" width=\"11\" height=\"11\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/17px-Increase2.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/22px-Increase2.svg.png 2x\" /></span> US$ 93.80&#160;billion (2012)<sup id=\"cite_ref-Google-Inc-Jan-2013-10-K_4-3\" class=\"reference\"><a href=\"#cite_note-Google-Inc-Jan-2013-10-K-4\"><span>[</span>4<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Equity_(finance)\" title=\"Equity (finance)\">Total equity</a></th><td>\n<span title=\"Increase\"><img alt=\"Increase\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/11px-Increase2.svg.png\" width=\"11\" height=\"11\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/17px-Increase2.svg.png 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Increase2.svg/22px-Increase2.svg.png 2x\" /></span> US$ 71.72&#160;billion (2012)<sup id=\"cite_ref-Google-Inc-Jan-2013-10-K_4-4\" class=\"reference\"><a href=\"#cite_note-Google-Inc-Jan-2013-10-K-4\"><span>[</span>4<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Employees</th><td>\n46,421 <small>(Q3 2013)</small><sup id=\"cite_ref-10K_5-0\" class=\"reference\"><a href=\"#cite_note-10K-5\"><span>[</span>5<span>]</span></a></sup></td></tr><tr><th scope=\"row\" style=\"text-align:left;\"><a href=\"/wiki/Subsidiary\" title=\"Subsidiary\">Subsidiaries</a></th><td>\nAdMob, DoubleClick, Motorola Mobility, On2 Technologies, Picnik, YouTube, Zagat, Waze, Blogger</td></tr><tr><th scope=\"row\" style=\"text-align:left;\">Website</th><td>\n<span class=\"url\"><a rel=\"nofollow\" class=\"external text\" href=\"https://www.google.com\">www.google.com</a></span></td></tr><tr><td colspan=\"2\" style=\"text-align:center;\">\n<b>References:</b> <sup id=\"cite_ref-form_10-k_6-0\" class=\"reference\"><a href=\"#cite_note-form_10-k-6\"><span>[</span>6<span>]</span></a></sup></td></tr></table>\n<p><b>Google</b> is an American <a href=\"/wiki/Multinational_corporation\" title=\"Multinational corporation\">multinational corporation</a> specializing in Internet-related services and products. These include <a href=\"/wiki/Web_search_engine\" title=\"Web search engine\">search</a>, <a href=\"/wiki/Cloud_computing\" title=\"Cloud computing\">cloud computing</a>, <a href=\"/wiki/Software\" title=\"Software\">software</a>, and <a href=\"/wiki/Online_advertising\" title=\"Online advertising\">online advertising</a> technologies.<sup id=\"cite_ref-See:_List_of_Google_products_7-0\" class=\"reference\"><a href=\"#cite_note-See:_List_of_Google_products-7\"><span>[</span>7<span>]</span></a></sup> Most of its profits are derived from <a href=\"/wiki/AdWords\" title=\"AdWords\">AdWords</a>.<sup id=\"cite_ref-financialtables_8-0\" class=\"reference\"><a href=\"#cite_note-financialtables-8\"><span>[</span>8<span>]</span></a></sup><sup id=\"cite_ref-9\" class=\"reference\"><a href=\"#cite_note-9\"><span>[</span>9<span>]</span></a></sup>\n</p><p>Google was founded by <a href=\"/wiki/Larry_Page\" title=\"Larry Page\">Larry Page</a> and <a href=\"/wiki/Sergey_Brin\" title=\"Sergey Brin\">Sergey Brin</a> while they were <a href=\"/wiki/Doctor_of_Philosophy\" title=\"Doctor of Philosophy\">Ph.D.</a> students at <a href=\"/wiki/Stanford_University\" title=\"Stanford University\">Stanford University</a>. Together they own about 16&#160;percent of its shares. They incorporated Google as a privately held company on September&#160;4, 1998. An <a href=\"/wiki/Initial_public_offering\" title=\"Initial public offering\">initial public offering</a> followed on August&#160;19, 2004. Its <a href=\"/wiki/Mission_statement\" title=\"Mission statement\">mission statement</a> from the outset was \"to organize the world's information and make it universally accessible and useful\",<sup id=\"cite_ref-corporate_10-0\" class=\"reference\"><a href=\"#cite_note-corporate-10\"><span>[</span>10<span>]</span></a></sup> and its unofficial slogan was \"<a href=\"/wiki/Don%27t_be_evil\" title=\"Don't be evil\">Don't be evil</a>\".<sup id=\"cite_ref-11\" class=\"reference\"><a href=\"#cite_note-11\"><span>[</span>11<span>]</span></a></sup><sup id=\"cite_ref-12\" class=\"reference\"><a href=\"#cite_note-12\"><span>[</span>12<span>]</span></a></sup> In 2006 Google moved to headquarters in <a href=\"/wiki/Mountain_View,_California\" title=\"Mountain View, California\">Mountain View, California</a>, nicknamed the <a href=\"/wiki/Googleplex\" title=\"Googleplex\">Googleplex</a>.\n</p>\n<div class=\"thumb tright\"><div class=\"thumbinner\" style=\"width:222px;\"><a href=\"/wiki/File:Ad-tech_London_2010_(2).JPG\" class=\"image\"><img alt=\"\" src=\"//upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Ad-tech_London_2010_%282%29.JPG/220px-Ad-tech_London_2010_%282%29.JPG\" width=\"220\" height=\"165\" class=\"thumbimage\" srcset=\"//upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Ad-tech_London_2010_%282%29.JPG/330px-Ad-tech_London_2010_%282%29.JPG 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Ad-tech_London_2010_%282%29.JPG/440px-Ad-tech_London_2010_%282%29.JPG 2x\" /></a>  <div class=\"thumbcaption\"><div class=\"magnify\"><a href=\"/wiki/File:Ad-tech_London_2010_(2).JPG\" class=\"internal\" title=\"Enlarge\"><img src=\"//bits.wikimedia.org/static-1.23wmf10/skins/common/images/magnify-clip.png\" width=\"15\" height=\"11\" alt=\"\" /></a></div>Google on ad-tech London, 2010</div></div></div>\n<p>Rapid growth since incorporation has triggered a chain of products, <a href=\"/wiki/Mergers_and_acquisitions\" title=\"Mergers and acquisitions\">acquisitions</a> and partnerships beyond <a href=\"/wiki/Google_Search\" title=\"Google Search\">Google's core search engine</a>. It offers online <a href=\"/wiki/Productivity_software\" title=\"Productivity software\">productivity software</a> including <a href=\"/wiki/Webmail\" title=\"Webmail\">email</a> (<a href=\"/wiki/Gmail\" title=\"Gmail\">Gmail</a>), an <a href=\"/wiki/Online_office_suite\" title=\"Online office suite\">office suite</a> (<a href=\"/wiki/Google_Drive\" title=\"Google Drive\">Google Drive</a>), and <a href=\"/wiki/Social_networking_service\" title=\"Social networking service\">social networking</a> (<a href=\"/wiki/Google%2B\" title=\"Google+\">Google+</a>). <a href=\"/wiki/Desktop_environment\" title=\"Desktop environment\">Desktop</a> products include applications for web browsing, organizing and <a href=\"/wiki/Graphics_software\" title=\"Graphics software\">editing photos</a>, and <a href=\"/wiki/Instant_messaging\" title=\"Instant messaging\">instant messaging</a>. The company leads the development of the <a href=\"/wiki/Android_(operating_system)\" title=\"Android (operating system)\">Android</a> mobile <a href=\"/wiki/Operating_system\" title=\"Operating system\">operating system</a> and the browser-only <a href=\"/wiki/Chrome_OS\" title=\"Chrome OS\">Chrome OS</a><sup id=\"cite_ref-13\" class=\"reference\"><a href=\"#cite_note-13\"><span>[</span>13<span>]</span></a></sup> for a <a href=\"/wiki/Netbook\" title=\"Netbook\">netbook</a> known as a <a href=\"/wiki/Chromebook\" title=\"Chromebook\">Chromebook</a>. Google has moved increasingly into communications hardware: it partners with major electronics manufacturers in production of its high-end <a href=\"/wiki/Google_Nexus\" title=\"Google Nexus\">Nexus</a> devices and acquired <a href=\"/wiki/Motorola_Mobility\" title=\"Motorola Mobility\">Motorola Mobility</a> in May 2012.<sup id=\"cite_ref-Hardware_company_14-0\" class=\"reference\"><a href=\"#cite_note-Hardware_company-14\"><span>[</span>14<span>]</span></a></sup> In 2012, a fiber-optic infrastructure was installed in <a href=\"/wiki/Kansas_City_metropolitan_area\" title=\"Kansas City metropolitan area\">Kansas City</a> to facilitate a <a href=\"/wiki/Google_Fiber\" title=\"Google Fiber\">Google Fiber</a> broadband service.<sup id=\"cite_ref-Google_Fiber_Cable_TV_Business_15-0\" class=\"reference\"><a href=\"#cite_note-Google_Fiber_Cable_TV_Business-15\"><span>[</span>15<span>]</span></a></sup>\n</p><p>The corporation has been estimated to run more than one million servers in data centers around the world<sup id=\"cite_ref-16\" class=\"reference\"><a href=\"#cite_note-16\"><span>[</span>16<span>]</span></a></sup> and to process over one billion search requests<sup id=\"cite_ref-17\" class=\"reference\"><a href=\"#cite_note-17\"><span>[</span>17<span>]</span></a></sup> and about 24 <a href=\"/wiki/Petabyte\" title=\"Petabyte\">petabytes</a> of user-generated data each day.<sup id=\"cite_ref-18\" class=\"reference\"><a href=\"#cite_note-18\"><span>[</span>18<span>]</span></a></sup><sup id=\"cite_ref-19\" class=\"reference\"><a href=\"#cite_note-19\"><span>[</span>19<span>]</span></a></sup><sup id=\"cite_ref-20\" class=\"reference\"><a href=\"#cite_note-20\"><span>[</span>20<span>]</span></a></sup><sup id=\"cite_ref-21\" class=\"reference\"><a href=\"#cite_note-21\"><span>[</span>21<span>]</span></a></sup>\nIn December 2013 <a href=\"/wiki/Alexa_Internet\" title=\"Alexa Internet\">Alexa</a> listed google.com as the most visited website in the world. Numerous Google sites in other languages figure in the top one hundred, as do several other Google-owned sites such as <a href=\"/wiki/YouTube\" title=\"YouTube\">YouTube</a> and <a href=\"/wiki/Blogger_(service)\" title=\"Blogger (service)\">Blogger</a>.<sup id=\"cite_ref-22\" class=\"reference\"><a href=\"#cite_note-22\"><span>[</span>22<span>]</span></a></sup> Its market dominance has led to prominent media coverage, including <a href=\"/wiki/Criticism_of_Google\" title=\"Criticism of Google\">criticism of the company</a> over issues such as <a href=\"/wiki/Criticism_of_Google#Copyright_issues\" title=\"Criticism of Google\">copyright</a>, <a href=\"/wiki/Censorship_by_Google\" title=\"Censorship by Google\">censorship</a>, and <a href=\"/wiki/Google_privacy\" title=\"Google privacy\" class=\"mw-redirect\">privacy</a>.<sup id=\"cite_ref-23\" class=\"reference\"><a href=\"#cite_note-23\"><span>[</span>23<span>]</span></a></sup><sup id=\"cite_ref-gatekeepers_24-0\" class=\"reference\"><a href=\"#cite_note-gatekeepers-24\"><span>[</span>24<span>]</span></a></sup>\n<br /><strong class=\"error mw-ext-cite-error\">Cite error: There are <code>&lt;ref&gt;</code> tags on this page, but the references will not show without a <code>&#123;&#123;reflist&#125;&#125;</code> template (see the <a href=\"/wiki/Help:Cite_errors/Cite_error_refs_without_references\" title=\"Help:Cite errors/Cite error refs without references\">help page</a>).</strong>\n</p>"
    val rep = "<[^<]+?>".r.replaceAllIn(a, ".").replaceAll("(\\.+)", ".").replaceAll("\n\\.\n", "\n")
    println(rep)
  }

  test("Produce descending series") {
    val n = 10
    val x = 2.0 / (n + 1)
    val series = (1 to 10).map(p => ((n - p + 1) / n.asInstanceOf[Float]) * x)

    println(series)

    assert(series.length == n)

    assert(math.abs(1 - series.sum) <= math.exp(5))
  }
  test("Test sum") {
    val l = List(
      0.09487179517745972,
      0.03333333432674408,
      0.08205128312110901,
      0.04871794879436493,
      0.025641027092933658,
      0.038461539149284366,
      0.1

    )

    println(l.sum)

    assert(l.sum == 0.42307692766189575)
  }

  test("Test NLP") {
    // create the processor
    val proc: Processor = new CoreNLPProcessor()

    // the actual work is done here
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

    // you are basically done. the rest of this code simply prints out the annotations

    // let's print the sentence-level annotations
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      println("Tokens: " + sentence.words.mkString(" "))
      println("Start character offsets: " + sentence.startOffsets.mkString(" "))
      println("End character offsets: " + sentence.endOffsets.mkString(" "))

      // these annotations are optional, so they are stored using Option objects, hence the foreach statement
      sentence.lemmas.foreach(lemmas => println("Lemmas: " + lemmas.mkString(" ")))
      sentence.tags.foreach(tags => println("POS tags: " + tags.mkString(" ")))
      sentence.entities.foreach(entities => println("Named entities: " + entities.mkString(" ")))
      sentence.norms.foreach(norms => println("Normalized entities: " + norms.mkString(" ")))
      sentence.dependencies.foreach(dependencies => {
        println("Syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.syntacticTree.foreach(tree => {
        println("Constituent tree: " + tree)
        // see the edu.arizona.sista.utils.Tree class for more information
        // on syntactic trees, including access to head phrases/words
      })

      sentenceCount += 1
      println("\n")
    }

    // let's print the coreference chains
    doc.coreferenceChains.foreach(chains => {
      for (chain <- chains.getChains) {
        println("Found one coreference chain containing the following mentions:")
        for (mention <- chain) {
          // note that all these offsets start at 0 too
          println("\tsentenceIndex:" + mention.sentenceIndex +
            " headIndex:" + mention.headIndex +
            " startTokenOffset:" + mention.startOffset +
            " endTokenOffset:" + mention.endOffset +
            " text: " + doc.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset)
                        .mkString("[", " ", "]"))
        }
      }
    })
  }

  test("Test sentence splitter") {
    import ir.phsys.util.SentenceUtils._
    val words = "Hi this morning.".tokenizeSentence()
    assert(words.equals(List("Hi", "this", "morning")))

    val sentences = "Hi. This is me".tokenizeText()

    assert(sentences.equals(List("Hi", "This is me")))
  }

  test("Test reduce") {

    def convert(str: String) = {
      str + ":"
    }

    val pattern="(\\ )|(\\.)"
    val tokens = "Hi this is a text".split(pattern).toList

    val reduce = tokens.reduce((a, b) => convert(a) + convert(b))
    println(reduce)

    val fold = tokens.fold("") {
      case (a, b) => convert(a) + convert(b)
    }
    println(fold)

    val scan = tokens.scan("") {
      case (a, b) => convert(a) + convert(b)
    }
    println(scan)


  }
}
