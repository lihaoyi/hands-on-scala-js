package torimatomeru
package syntax

import org.parboiled2._

trait Literals extends StringLiterals { self: ScalaSyntax =>

  def FloatingPointLiteral = rule {
    capture(
      "." ~ oneOrMore(Digit) ~ optional(ExponentPart) ~ optional(FloatType) |
        oneOrMore(Digit) ~ (
          "." ~ oneOrMore(Digit) ~ optional(ExponentPart) ~ optional(FloatType) |
          ExponentPart ~ optional(FloatType) |
          optional(ExponentPart) ~ FloatType))
  }

  def IntegerLiteral = rule { capture((DecimalNumeral | HexNumeral) ~ optional(anyOf("Ll"))) }

  def BooleanLiteral = rule { capture("true" | "false") }

  def MultilineComment: Rule0 = rule { "/*" ~ zeroOrMore(MultilineComment | !"*/" ~ ANY) ~ "*/" }
  def Comment: Rule0 = rule {
    MultilineComment |
      "//" ~ zeroOrMore(!NewlineS ~ ANY) ~ (NewlineS | EOI)
  }

  def Literal = rule {
    (capture(optional("-")) ~ (FloatingPointLiteral | IntegerLiteral) ~> ((sign: String, number) => sign + number)) |
      BooleanLiteral |
      CharacterLiteral |
      StringLiteral |
      SymbolLiteral |
      capture("null")
  }
}

/**
 * Placed the string defintions in this trait to isolate them, because they are overly complex.
 */
private[syntax] trait StringLiterals { self: Literals with ScalaSyntax =>

  def EscapedChars = rule { '\\' ~ anyOf("rnt\\\"") }

  def SymbolLiteral = rule { ''' ~ capture(PlainId) }

  def CharacterLiteral = rule { ''' ~ capture(UnicodeExcape | EscapedChars | !'\\' ~ CharPredicate.from(isPrintableChar)) ~ ''' }

  def MultiLineChars = rule { zeroOrMore(optional('"') ~ optional('"') ~ noneOf("\"")) }
  def StringLiteral = rule {
    ("\"\"\"" ~ capture(MultiLineChars) ~ capture("\"\"\"" ~ zeroOrMore('"')) ~> ((multilineChars: String, quotes) => multilineChars + quotes.dropRight(3))) |
      ('"' ~ capture(zeroOrMore("\\\"" | noneOf("\n\""))) ~ '"')
  }

  def isPrintableChar(c: Char): Boolean = {
    val block = Character.UnicodeBlock.of(c)
    !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
  }
}
