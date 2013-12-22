package ast

case class ComponentValue(value: String)

case class Declaration(ident: String, expression: String, important: Boolean) {
  def genCSS: String = {
    val imp: String = (if (important) "!important" else "")
    ident + ": " + expression + imp + ";"
  }
}

case class Charset(value: String) {
  def genCSS = "@charset " + value + ";"
}

case class Import(value: String) {
  def genCSS: String = s"""@import "$value" """
}

trait Rule {
  def genCSS: String
}

case class FontFace(declarations: Seq[Declaration]) extends Rule {
  def genCSS: String =
    s"""
      |@font-face {
      |${declarations.map("  " + _.genCSS).mkString("\n")}
      |}
      |""".stripMargin
}

case class MediaRule(query: String, rules: Seq[QualifiedRule]) extends Rule {
  def genCSS =
    s"""
      |@media $query {
      |${rules.map(_.genCSS).mkString("\n")}
      |}
      |""".stripMargin
}

case class PageRule(pseudoPage: Option[String], rules: Seq[Declaration]) extends Rule {
  def genCSS =
    s""""
      |@page ${pseudoPage.getOrElse("")}
      |${rules.map(_.genCSS).mkString("\n")}
      |""".stripMargin
}

case class QualifiedRule(selector: Selector, declarations: Seq[Declaration]) extends Rule {
  def genCSS: String =
    s"""
      |${selector.value} {
      |${declarations.map("  " + _.genCSS).mkString("\n")}
      |}
    """.stripMargin
}

case class Selector(value: String)

case class Stylesheet(charset: Option[Charset], imports: Seq[Import], rules: Seq[Rule]) {

  def genCSS: String = {
    charset.map(_.genCSS + "\n").getOrElse("") +
    imports.map(_.genCSS).mkString("\n") +
    rules.map(_.genCSS).mkString("\n")
  }

}

