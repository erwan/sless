package test

import org.specs2.mutable._

class CSSParserSpec extends Specification {

  val simpleCSS =
    """
      |@import "main.css";
      |
      |body {
      |   text-color: #FF0000;
      |   font-size: 10px;
      |}
      |
      |.content.stuff {
      |   border: 10px solid black;
      |}
      |
    """.stripMargin

  val badCSS =
    """
      |body {
      |   text-color: #FF0000;
    """.stripMargin

  "A simple CSS" should {
    "be parsed" in {
      println(sless.CSSParser.parse(simpleCSS).map(_.genCSS))
      println(sless.CSSParser.parse(badCSS))
      1 must be equalTo 1
    }
  }

}
