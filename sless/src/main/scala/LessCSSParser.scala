package sless

import models._

// TODO: Mixin
// TODO: LessAST
object LessCSSParser extends sless.CSSParser {

  def constantName = "@[a-zA-Z_][a-zA-Z0-9_-]*".r
  def constantDef = constantName ~ ":" ~ term ~ ";"

  override def stylesheet = opt(charset_rule) ~ rep(import_rule) ~
    rep( media | page | font_face | ruleset /* | constantDef */) ^^ {
    case charset ~ imports ~ rules => Stylesheet(charset, imports.map(Import(_)), rules)
  }

}
