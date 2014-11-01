package scalatex.stages

import acyclic.file
import scala.annotation.elidable
import scala.annotation.elidable._
import scala.Some

import scala.collection.mutable.{ArrayBuffer, Buffer, ListBuffer}

object TwistNodes{
  trait Positioned{
    def offset: Int
  }
  abstract class TemplateTree extends Positioned
  abstract class ScalaExpPart extends Positioned

  case class Params(code: String, offset: Int) extends Positioned
  case class Template(name: PosString, comment: Option[Comment], params: PosString, topImports: Seq[Simple], imports: Seq[Simple], sub: Seq[Template], content: Seq[TemplateTree], offset: Int) extends Positioned
  case class PosString(str: String, offset: Int) extends Positioned {
    override def toString = str
  }
  case class Plain(text: String, offset: Int) extends TemplateTree with Positioned
  case class Display(exp: ScalaExp, offset: Int) extends TemplateTree with Positioned
  case class Comment(msg: String, offset: Int) extends TemplateTree with Positioned
  case class ScalaExp(parts: Seq[ScalaExpPart], offset: Int) extends TemplateTree with Positioned
  case class Simple(code: String, offset: Int) extends ScalaExpPart with Positioned
  case class Block(whitespace: String, args: Option[PosString], content: Seq[TemplateTree], offset: Int) extends ScalaExpPart with Positioned
}

import TwistNodes._

object Parser extends (String => Template){
  def apply(source: String) = {
    Parser.parse(source, _.template()) match{
      case Parser.Success(tmpl: Template, input) => tmpl
      case Parser.Error(input, errors) => throw new Exception("Parsing Failed " + errors.mkString("\n"))
    }
  }

  def parse[T](source: String, f: Parser => T): ParseResult[T] = {
    val p = new Parser

    // Initialize mutable state

    p.input.reset(source)
    p.errorStack.clear()
    val res = f(p)

    if (p.errorStack.length == 0 && res != null) Success(res, p.input)
    else Error(p.input, p.errorStack.toList)
  }

  sealed abstract class ParseResult[+T]{
    def toOption: Option[T]
  }
  case class Success[+T](t: T, input: Input) extends ParseResult[T]{
    def toOption = Some(t)
  }
  case class Error(input: Input, errors: List[PosString]) extends ParseResult[Nothing]{
    def toOption = None
  }

  case class Input() {
    private var offset_ = 0
    private var source_ = ""
    private var length_ = 1
    val regressionStatistics = new collection.mutable.HashMap[String, (Int, Int)]

    /** Peek at the current input. Does not check for EOF. */
    def apply(): Char = source_.charAt(offset_)

    /**
     * Peek `length` characters ahead. Does not check for EOF.
     * @return string from current offset upto current offset + `length`
     */
    def apply(length: Int): String = source_.substring(offset_, (offset_ + length))

    /** Equivalent to `input(str.length) == str`. Does not check for EOF. */
    def matches(str: String): Boolean = {
      var i = 0;
      val l = str.length
      while (i < l) {
        if (source_.charAt(offset_ + i) != str.charAt(i))
          return false
        i += 1
      }
      true
    }

    /** Advance input by one character */
    def advance(): Unit = offset_ += 1

    /** Advance input by `increment` number of characters */
    def advance(increment: Int): Unit = offset_ += increment

    /** Backtrack by `decrement` numner of characters */
    def regress(decrement: Int): Unit = offset_ -= decrement

    /** Backtrack to a known offset */
    def regressTo(offset: Int): Unit = {
      @noinline @elidable(INFO)
      def updateRegressionStatistics() = {
        val distance = offset_ - offset
        val methodName = Thread.currentThread().getStackTrace()(2).getMethodName()
        val (count, charAccum) = regressionStatistics.get(methodName) getOrElse ((0, 0))
        regressionStatistics(methodName) = (count + 1, charAccum + distance)
      }

      offset_ = offset
    }

    def isPastEOF(len: Int): Boolean = (offset_ + len-1) >= length_

    def isEOF() = isPastEOF(1)

    def atEnd() = isEOF()

    def offset() = offset_

    def source() = source_

    /** Reset the input to have the given contents */
    def reset(source: String) {
      offset_ = 0
      source_ = source
      length_ = source.length()
      regressionStatistics.clear()
    }
  }
}
class Parser{
  import Parser._

  val input: Input = new Input
  val errorStack: ListBuffer[PosString] = ListBuffer()

  /**
   *  Try to match `str` and advance `str.length` characters.
   *
   *  Reports an error if the input does not match `str` or if `str.length` goes past the EOF.
   */
  def accept(str: String): Unit = {
    val len = str.length
    if (!input.isPastEOF(len) && input.matches(str))
      input.advance(len)
    else
      error("Expected '" + str + "' but found '" + (if (input.isPastEOF(len)) "EOF" else input(len)) + "'")
  }

  /**
   *  Does `f` applied to the current peek return true or false? If true, advance one character.
   *
   *  Will not advance if at EOF.
   *
   *  @return true if advancing, false otherwise.
   */
  def check(f: Char => Boolean): Boolean = {
    if (!input.isEOF() && f(input())) {
      input.advance()
      true
    } else false
  }

  /**
   *  Does the current input match `str`? If so, advance `str.length`.
   *
   *  Will not advance if `str.length` surpasses EOF
   *
   *  @return true if advancing, false otherwise.
   */
  def check(str: String): Boolean = {
    val len = str.length
    if (!input.isPastEOF(len) && input.matches(str)){
      input.advance(len)
      true
    } else false
  }

  def error(message: String, offset: Int = input.offset): Unit = {
    errorStack += PosString(message, offset)
  }

  /** Consume/Advance `length` characters, and return the consumed characters. Returns "" if at EOF. */
  def any(length: Int = 1): String = {
    if (input.isEOF()) {
      error("Expected more input but found 'EOF'")
      ""
    } else {
      val s = input(length)
      input.advance(length)
      s
    }
  }

  /**
   *  Consume characters until input matches `stop`
   *
   *  @param inclusive - should stop be included in the consumed characters?
   *  @return the consumed characters
   */
  def anyUntil(stop: String, inclusive: Boolean): String = {
    var sb = new StringBuilder
    while (!input.isPastEOF(stop.length) && !input.matches(stop))
      sb.append(any())
    if (inclusive && !input.isPastEOF(stop.length))
      sb.append(any(stop.length))
    sb.toString()
  }

  /**
   *  Consume characters until `f` returns false on the peek of input.
   *
   *  @param inclusive - should the stopped character be included in the consumed characters?
   *  @return the consumed characters
   */
  def anyUntil(f: Char => Boolean, inclusive: Boolean): String = {
    var sb = new StringBuilder
    while (!input.isEOF() && f(input()) == false)
      sb.append(any())
    if (inclusive && !input.isEOF())
      sb.append(any())
    sb.toString
  }

  /** Recursively match pairs of prefixes and suffixes and return the consumed characters
    *
    *  Terminates at EOF.
    */
  def recursiveTag(prefix: String, suffix: String, allowStringLiterals: Boolean = false): String = {
    if (check(prefix)) {
      var stack = 1
      val sb = new StringBuffer
      sb.append(prefix)
      while (stack > 0) {
        if (check(prefix)) {
          stack += 1
          sb.append(prefix)
        } else if (check(suffix)) {
          stack -= 1
          sb.append(suffix)
        } else if (input.isEOF()) {
          error("Expected '" + suffix + "' but found 'EOF'")
          stack = 0
        } else if (allowStringLiterals) {
          stringLiteral("\"", "\\") match {
            case null => sb.append(any())
            case s => sb.append(s)
          }
        } else {
          sb.append(any())
        }
      }
      sb.toString()
    } else null
  }

  /**
   * Match a string literal, allowing for escaped quotes.
   * Terminates at EOF.
   */
  def stringLiteral(quote: String, escape: String): String = {
    if (check(quote)) {
      var within = true
      val sb = new StringBuffer
      sb.append(quote)
      while (within) {
        if (check(quote)) { // end of string literal
          sb.append(quote)
          within = false
        } else if (check(escape)) {
          sb.append(escape)
          if (check(quote)) { // escaped quote
            sb.append(quote)
          } else if (check(escape)) { // escaped escape
            sb.append(escape)
          }
        } else if (input.isEOF()) {
          error("Expected '" + quote + "' but found 'EOF'")
          within = false
        } else {
          sb.append(any())
        }
      }
      sb.toString()
    } else null
  }

  /** Match zero or more `parser` */
  def several[T, BufferType <: Buffer[T]](parser: () => T, provided: BufferType = null)(implicit manifest: Manifest[BufferType]): BufferType = {

    val ab = if (provided != null) provided else manifest.runtimeClass.newInstance().asInstanceOf[BufferType]
    var parsed = parser()
    while (parsed != null) {
      ab += parsed
      parsed = parser()
    }
    ab
  }

  def parentheses(): String = recursiveTag("(", ")", allowStringLiterals = true)

  def squareBrackets(): String = recursiveTag("[", "]")

  def whitespace(): String = anyUntil(_ > '\u0020', inclusive = false)

  // not completely faithful to original because it allows for zero whitespace
  def whitespaceNoBreak(): String = anyUntil(c => c != ' ' && c != '\t', inclusive = false)

  def identifier(): String = {
    var result: String = null
    // TODO: should I be checking for EOF here?
    if (!input.isEOF() && Character.isJavaIdentifierStart(input())) {
      result = anyUntil(Character.isJavaIdentifierPart(_) == false, false)
    }
    result
  }

  def comment(): Comment = {
    val pos = input.offset
    if (check("@*")) {
      val text = anyUntil("*@", inclusive = false)
      accept("*@")
      Comment(text, pos)
    } else null
  }

  def startArgs(): String = {
    val result = several[String, ArrayBuffer[String]](parentheses)
    if (result.length > 0)
      result.mkString
    else
      null
  }

  def importExpression(): Simple = {
    val p = input.offset
    if (check("@import "))
      Simple("import " + anyUntil("\n", inclusive = true).trim, p+1) // don't include position of @
    else null
  }

  def scalaBlock(): Simple = {
    if (check("@{")) {
      input.regress(1); // we need to parse the '{' via 'brackets()'
      val p = input.offset
      brackets() match {
        case null => null
        case b => Simple(b, p)
      }
    } else null
  }

  def brackets(): String = {
    var result = recursiveTag("{", "}")
    // mimicking how the original parser handled EOF for this rule
    if (result != null && result.last != '}')
      result = null
    result
  }

  def mixed(): ListBuffer[TemplateTree] = {
    // parses: comment | scalaBlockDisplayed | forExpression | matchExpOrSafeExprOrExpr | caseExpression | plain
    def opt1(): ListBuffer[TemplateTree] = {
      val t =
        comment() match {
          case null => scalaBlockDisplayed() match {
            case null => forExpression match {
              case null => matchExpOrSafeExpOrExpr() match {
                case null => caseExpression() match {
                  case null => plain()
                  case x => x
                }
                case x => x
              }
              case x => x
            }
            case x => x
          }
          case x => x
        }
      if (t != null) ListBuffer(t)
      else null
    }

    // parses: '{' mixed* '}'
    def opt2(): ListBuffer[TemplateTree] = {
      val lbracepos = input.offset()
      if (check("{")) {
        var buffer = new ListBuffer[TemplateTree]
        buffer += Plain("{", lbracepos)
        for (m <- several[ListBuffer[TemplateTree], ListBuffer[ListBuffer[TemplateTree]]](mixed))
          buffer = buffer ++ m // creates a new object, but is constant in time, as opposed to buffer ++= m which is linear (proportional to size of m)
        val rbracepos = input.offset
        if (check("}"))
          buffer += Plain("}", rbracepos)
        else
          error("Expected ending '}'")
        buffer
      } else null
    }

    opt1() match {
      case null => opt2()
      case x => x
    }
  }

  def scalaBlockDisplayed(): Display = {
    val sb = scalaBlock()

    if (sb != null)
      Display(ScalaExp(sb :: Nil, sb.offset), sb.offset)
    else
      null
  }

  def blockArgs(): PosString = {
    val p = input.offset
    val result = anyUntil("=>", true)
    if (result.endsWith("=>") && !result.contains("\n"))
      PosString(result, p)
    else {
      input.regress(result.length())
      null
    }
  }

  def block(): Block = {
    var result: Block = null
    val p = input.offset
    val ws = whitespaceNoBreak()
    if (check("{")) {
      val blkArgs = Option(blockArgs())
      val mixeds = several[ListBuffer[TemplateTree], ListBuffer[ListBuffer[TemplateTree]]](mixed)
      accept("}")
      // TODO - not use flatten here (if it's a performance problem)
      result = Block(ws, blkArgs, mixeds.flatten, p)
    } else {
      input.regressTo(p)
    }

    result
  }

  def caseExpression(): TemplateTree = {
    var result: TemplateTree = null

    val wspos = input.offset
    val ws = whitespace()
    val p = input.offset()
    if (check("case ")) {
      val pattern = Simple("case " + anyUntil("=>", inclusive = true), p)
      val blk = block()
      if (blk != null) {
        result = ScalaExp(ListBuffer(pattern, blk), blk.offset)
        whitespace()
      } else {
        //error("Expected block after 'case'")
        input.regressTo(wspos)
      }
    } else if (ws.length > 0) {
      // We could regress here and not return something for the ws, because the plain production rule
      // would parse this, but this would actually be a hotspot for backtracking, so let's return it
      // here seeing as it's been parsed all ready.
      result = Plain(ws, wspos)
    }

    result
  }

  def matchExpOrSafeExpOrExpr(): Display = {
    val resetPosition = input.offset
    val result =
      expression() match {
        case null => safeExpression()
        case x => x
      }

    if (result != null) {
      val exprs = result.exp.parts.asInstanceOf[ListBuffer[ScalaExpPart]]
      val mpos = input.offset
      val ws = whitespaceNoBreak()
      if (check("match")) {
        val m = Simple(ws + "match", mpos)
        val blk = block()
        if (blk != null) {
          exprs.append(m)
          exprs.append(blk)
        } else {
          // error("expected block after match")
          input.regressTo(mpos)
        }
      } else {
        input.regressTo(mpos)
      }
    }

    result
  }

  def forExpression(): Display = {
    var result: Display = null
    val p = input.offset
    if (check("@for")) {
      val parens = parentheses()
      if (parens != null) {
        val blk = block()
        if (blk != null) {
          result = Display(ScalaExp(ListBuffer(Simple("for" + parens + " yield ", p+1), blk), p+1), p+1) // don't include pos of @
        }
      }
    }

    if (result == null)
      input.regressTo(p)

    result
  }

  def safeExpression(): Display = {
    if (check("@(")) {
      input.regress(1)
      val p = input.offset
      Display(ScalaExp(ListBuffer(Simple(parentheses(), p)), p), p)
    } else null
  }

  def plain(): Plain = {
    def single(): String = {
      if (check("@@")) "@"
      else if (!input.isEOF() && input() != '@' && input() != '}' && input() != '{') any()
      else null
    }
    val p = input.offset
    var result: Plain = null
    var part = single()
    if (part != null) {
      val sb = new StringBuffer
      while (part != null) {
        sb.append(part)
        part = single()
      }
      result = Plain(sb.toString(), p)
    }

    result
  }

  def expression(): Display = {
    var result: Display = null
    if (check("@")) {
      val pos = input.offset
      val code = methodCall()
      if (code != null) {
        val parts = several[ScalaExpPart, ListBuffer[ScalaExpPart]](expressionPart)
        parts.prepend(Simple(code, pos))
        result = Display(ScalaExp(parts, pos-1), pos-1)
      } else {
        input.regressTo(pos - 1) // don't consume the @ if we fail
      }
    }

    result
  }

  def methodCall(): String = {
    val name = identifier()
    if (name != null) {
      val sb = new StringBuffer(name)
      sb.append(Option(squareBrackets) getOrElse "")
      sb.append(Option(parentheses) getOrElse "")
      sb.toString()
    } else null
  }

  def expressionPart(): ScalaExpPart = {
    def simpleParens() = {
      val p = input.offset
      val parens = parentheses()
      if (parens != null) Simple(parens, p)
      else null
    }

    def wsThenScalaBlockChained() = {
      val reset = input.offset
      val ws = whitespaceNoBreak()
      val chained = scalaBlockChained()
      if (chained eq null) input.regressTo(reset)
      chained
    }

    chainedMethods() match {
      case null => block() match {
        case null => wsThenScalaBlockChained() match {
          case null => elseCall() match {
            case null => simpleParens()
            case x => x
          }
          case x => x
        }
        case x => x
      }
      case x => x
    }
  }

  def scalaBlockChained(): Block = {
    val blk = scalaBlock()
    if (blk != null)
      Block("", None, ListBuffer(ScalaExp(ListBuffer(blk), blk.offset)), blk.offset)
    else null
  }

  def chainedMethods(): Simple = {
    val p = input.offset
    var result: Simple = null
    if (check(".")) {
      val firstMethodCall = methodCall()
      if (firstMethodCall != null) {
        val sb = new StringBuffer("." + firstMethodCall)
        var done = false
        while (!done) {
          val reset = input.offset
          var nextLink: String = null
          if (check(".")) {
            methodCall() match {
              case m: String => nextLink = m
              case _ =>
            }
          }

          nextLink match {
            case null => {
              done = true
              input.regressTo(reset)
            }
            case _ => {
              sb.append(".")
              sb.append(nextLink)
            }
          }
        }

        result = Simple(sb.toString(), p)
      } else input.regressTo(p)
    }

    result
  }

  def elseCall(): Simple = {
    val reset = input.offset
    whitespaceNoBreak()
    val p = input.offset
    if (check("else")) {
      whitespaceNoBreak()
      Simple("else", p)
    } else {
      input.regressTo(reset)
      null
    }
  }

  def template(): Template = {
    val topImports = extraImports()
    whitespace()
    val commentpos = input.offset
    val cm = Option(comment()).map(_.copy(offset = commentpos))
    whitespace()
    val args =
      if (check("@(")) {
        input.regress(1)
        val p = input.offset
        val args = startArgs()
        if (args != null) Some(PosString(args, p))
        else None
      } else None
    val (imports, templates, mixeds) = templateContent()

    Template(PosString("", 0), cm, args.getOrElse(PosString("()", 0)), topImports, imports, templates, mixeds, 0)
  }
  def subTemplate(): Template = {
    var result: Template = null
    val resetPosition = input.offset
    val templDecl = templateDeclaration()
    if (templDecl != null) {
      anyUntil(c => c != ' ' && c != '\t', inclusive = false)
      if (check("{")) {
        val (imports, templates, mixeds) = templateContent()
        if (check("}"))
          result = Template(templDecl._1, None, templDecl._2, Nil, imports, templates, mixeds, templDecl._1.offset)
      }
    }

    if (result == null)
      input.regressTo(resetPosition)
    result
  }

  def templateDeclaration(): (PosString, PosString) = {
    if (check("@")) {
      val namepos = input.offset
      val name = identifier() match {
        case null => null
        case id => PosString(id, namepos)
      }

      if (name != null) {
        val paramspos = input.offset
        val types = Option(squareBrackets) getOrElse ""
        val args = several[String, ArrayBuffer[String]](parentheses)
        val params = PosString(types + args.mkString, paramspos)
        if (params != null)

          anyUntil(c => c != ' ' && c != '\t', inclusive = false)
          if (check("=")) {
            return (name, params)
          }
      } else input.regress(1) // don't consume @
    }

    null
  }

  def templateContent(): (Seq[Simple], Seq[Template], Seq[TemplateTree]) = {
    val imports = new ArrayBuffer[Simple]

    val templates = new ArrayBuffer[Template]
    val mixeds = new ArrayBuffer[TemplateTree]

    var done = false
    while (!done) {
      val impExp = importExpression()
      if (impExp != null) imports += impExp
      else {

        val templ = subTemplate()
        if (templ != null) templates += templ
        else {
          val mix = mixed()
          if (mix != null) mixeds ++= mix
          else {
            // check for an invalid '@' symbol, and just skip it so we can continue the parse
            val pos = input.offset
            if (check("@")) error("Invalid '@' symbol", pos)
            else done = true
          }
        }
      }
    }

    (imports, templates, mixeds)
  }

  def extraImports(): Seq[Simple] = {
    val resetPosition = input.offset
    val imports = new ArrayBuffer[Simple]

    while (whitespace().nonEmpty || (comment() ne null)) {} // ignore

    var done = false
    while (!done) {
      val importExp = importExpression()
      if (importExp ne null) {
        imports += importExp
        whitespace()
      } else {
        done = true
      }
    }

    if (imports.isEmpty) {
      input.regressTo(resetPosition)
    }

    imports
  }



  def mkRegressionStatisticsString() {
    val a = input.regressionStatistics.toArray.sortBy { case (m, (c, a)) => c }
    a.mkString("\n")
  }

  // TODO - only for debugging purposes, remove before release
  def setSource(source: String) {
    input.reset(source)
  }
}