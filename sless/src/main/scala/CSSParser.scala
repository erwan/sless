package sless

import util.parsing.combinator._

import ast._

// TODO: Attributes
// TODO: CSS Transforms

trait CSSParser extends RegexParsers {
  def stringLiteral = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt"]|\\u[a-fA-F0-9]{4})*"""+"\"").r | ("\'"+"""([^'\p{Cntrl}\\]|\\[\\/bfnrt']|\\u[a-fA-F0-9]{4})*"""+"\'").r
  def ident = """[*@_]?-?[a-zA-Z_][a-zA-Z0-9_-]*""".r
  def decimalNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)""".r

  // Lexical symbols
  def IMPORT_SYM = "(?i)@import".r
  def PAGE_SYM = "(?i)@page".r
  def MEDIA_SYM = "(?i)@media".r
  def FONT_FACE_SYM = "(?i)@font-face".r
  def CHARSET_SYM = "(?i)@charset".r
  def NAMESPACE_SYM = "(?i)@namespace".r
  def IMPORTANT_SYM = "!important" | ("!" ~ "important")
  def UNIT = "(?i)(?:px|cm|mm|in|pt|pc|em|ex|dpi|s|ms|Hz|kHz|%)".r
  def unary_operator: Parser[String] = "-" | "+"
  def operator = "/" | ","
  def combinator: Parser[String] = "+" | ">" | "~"
  def EXP = """[eE][\+\-]?\d+""".r
  def NUMBER: Parser[String] = opt(unary_operator) ~ decimalNumber ~ opt(EXP) ^^ {
    case op ~ num ~ exp => op.getOrElse("") + num + exp.getOrElse("")
  }
  def URI: Parser[String] = "url([^)]+)".r
  def hexcolor: Parser[String] = "#(?:[0-9A-Fa-f]{3}){1,2}".r

  // GRAMMAR
  def componentvalue: Parser[String] = decimalNumber ~ UNIT ^^ { case num ~ unit => (num + unit) }
  def term: Parser[String] = opt(unary_operator) ~ (componentvalue | URI | hexcolor | stringLiteral | NUMBER | ident) ^^ {
    case unaryOpt ~ value => unaryOpt.getOrElse("") + value
  }
  def declaration: Parser[Declaration] = (ident <~ ":") ~ "[^;!]+".r ~ (opt(IMPORT_SYM) <~ ";") ^^ {
    case prop ~ expr ~ important => Declaration(prop, expr, important.isDefined)
  }
  def element_selector: Parser[Selector] = """\w*([#\.\:]\w+)*""".r ^^ { case eltsel => Selector(eltsel) }
  def selector: Parser[Selector] = element_selector | (element_selector ~ opt(combinator) ~ selector) ^^ {
    case s1 ~ comb ~ s2 => Selector(Seq(s1, comb.getOrElse(""), s2).mkString(" "))
  }
  def declaration_body: Parser[Seq[Declaration]] = "{" ~> rep(declaration) <~ "}"
  def charset_rule: Parser[Charset] = CHARSET_SYM ~> stringLiteral <~ ";" ^^ (Charset(_))
  def ruleset: Parser[QualifiedRule] = selector ~ declaration_body ^^ {
    case selectors ~ body => QualifiedRule(selectors, body)
  }

  // AT-RULES
  def pseudo_page: Parser[String] = ":" ~> ident ^^ (":" + _)
  def page: Parser[PageRule] = PAGE_SYM ~> opt(pseudo_page) ~ declaration_body ^^ { case pseudo ~ body => PageRule(pseudo, body) }
  def media_qualifier: Parser[String] = ident | ("(" ~ ident ~ ":" ~ term ~ ")") ^^ { case _ ~ id ~ _ ~ t ~ _ => s"($id:$t)" }
  def media_query: Parser[String] = media_qualifier | (media_qualifier ~ "and" ~ media_query) ^^ { case q ~ _ ~ t => s"$q and $t" }
  def media: Parser[MediaRule] = MEDIA_SYM ~ media_query ~ "{" ~ rep(ruleset) ~ "}" ^^ { case _ ~ query ~ _ ~ rules ~ _ => MediaRule(query, rules) }
  def import_rule: Parser[String] = IMPORT_SYM ~> stringLiteral.map(_.drop(1).dropRight(1)) <~ ";"
  def font_face: Parser[FontFace] = FONT_FACE_SYM ~> declaration_body ^^ { case decs => FontFace(decs) }

  // Entry point
   def stylesheet: Parser[Stylesheet] = opt(charset_rule) ~ rep(import_rule) ~
    rep( media | page | font_face | ruleset /* | constantDef */) ^^ {
    case charset ~ imports ~ rules => Stylesheet(charset, imports.map(Import(_)), rules)
  }

  def parse(in: java.io.Reader) = parseAll(stylesheet, in)
  def parse(in: java.lang.CharSequence) = parseAll(stylesheet, in)
}

object CSSParser extends CSSParser
