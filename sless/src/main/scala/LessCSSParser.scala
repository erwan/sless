package sless

import ast._

// TODO: Mixin
// TODO: LessAST
object LessCSSParser extends sless.CSSParser {

  def constantName = "@[a-zA-Z_][a-zA-Z0-9_-]*".r
  def constantDef: Parser[Constant] = constantName ~ (":" ~> term <~ ";") ^^ { case name ~ term => Constant(name, term) }

  def lessStylesheet = opt(charset_rule) ~ rep(import_rule) ~
    rep( media | page | font_face | ruleset | constantDef) ^^ {
    case charset ~ imports ~ rules => LessStylesheet(charset, imports.map(Import(_)), rules)
  }

}
