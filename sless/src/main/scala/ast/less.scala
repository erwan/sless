package ast

case class Constant(name: String, term: String) extends Rule {
  def genCSS = ""
}

case class LessStylesheet(charset: Option[Charset], imports: Seq[Import], rules: Seq[Rule]) {

  def compile() = Stylesheet(charset, Nil, LessStylesheet.compiledRules(rules))

}

object LessStylesheet {
  def compiledRules(rules: Seq[Rule]) = {
    rules.foldLeft(RichRules.empty) { (res: RichRules, rule: Rule) =>
      rule match {
        case Constant(n, t) if res.constants.get(n).isDefined => sys.error(s"Less Compilation: $n already defined")
        case Constant(n, t) => res.copy(constants = res.constants + (n -> t))
        case rule => res.copy(rules = res.rules :+ rule)
      }
    }.rules
  }

  case class RichRules(rules: Seq[Rule], constants: Map[String, String])

  object RichRules {
    val empty = RichRules(Nil, Map.empty)
  }

}
