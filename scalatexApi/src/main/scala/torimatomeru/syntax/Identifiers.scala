package torimatomeru
package syntax

import org.parboiled2._

trait Identifiers { self: Parser with Basic =>
  object Identifiers{
    import Basic._
    def Operator = rule(oneOrMore(OperatorChar))

    def VarId = rule { !(Keywords ~ (WhitespaceChar | Newline | "//" | "/*")) ~ Lower ~ IdRest }
    def PlainId = rule { Upper ~ IdRest | VarId | !(Keywords ~ (WhitespaceChar | Newline | "//" | "/*")) ~ Operator }
    def Id = rule { PlainId | ("`" ~ oneOrMore(noneOf("`")) ~ "`") }
    def IdRest = rule { zeroOrMore(Letter | Digit) ~ optional("_" ~ Operator) }


    def Keywords = rule {
      "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" | "extends" | "false" | "finally" | "final" | "finally" | "forSome" | "for" | "if" |
      "implicit" | "import" | "lazy" | "match" | "new" | "null" | "object" | "override" | "package" | "private" | "protected" | "return" |
      "sealed" | "super" | "this" | "throw" | "trait" | "try" | "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_" |
      ":" | ";" | "=>" | "=" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
    }
  }
}
