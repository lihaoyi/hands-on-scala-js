package twist.stages

import acyclic.file
/**
 * Implements the offside-rule for Twirlite fragments.
 *
 * This stage walks over the whitespace before each line of the Twirlite,
 * augmenting it with the corresponding open-{ and close-} such that an
 * un-modified Twirl parser can then parse it.
 *
 * The rule is simple: any line which starts with an expression potentially
 * opens a new block. Any text after the end of the expression is inside the
 * block, as well as any text on subsequent lines which are indented more
 * deeply.
 */
object IndentHandler extends (String => String){
  def noBraceLine(remainder: String) = {
    !remainder.trim.headOption.exists("{(".toSeq.contains)
  }

  def successRemainder(r: Parser.ParseResult[_]) = r match {
    case Parser.Error(_, _) => None
    case Parser.Success(_, input) => Some(input.source().take(input.offset()))
  }

  def apply(input: String): String = {

    val lines = "" :: input.lines.toList ::: List("")
    val tails = lines.tails.takeWhile(_ != Nil)

    def getIndent(rest: List[String]): (Int, String) = rest match {
      case Nil => (0, "")
      case x if x.head.trim() != "" => (x.head.takeWhile(_ == ' ').length, x.head)
      case _ => getIndent(rest.tail)
    }
    val min = input.lines
      .map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min

    val linesOut = tails.foldLeft((min :: Nil, Seq[String]())){ (x, tail) =>
      val (stack, text) = x
      val spacedCurrent :: next = tail
      val (spaces, current) = spacedCurrent.splitAt(
        math.max(spacedCurrent.indexWhere(_ != ' '), 0)
      )

//      println("index " + math.max(spacedCurrent.indexWhere(_ != ' '), 0))
//      println(spaces, current)

      val declRemainder = successRemainder(Parser.parse(current.trim, _.templateDeclaration()))

      val exprRemainder = successRemainder(Parser.parse(current.trim, _.expression())).filter(_ == current.trim)


      /**
       * Whether or not the line starts with some sort of indent-start-marker, and
       * if it does what the remainder of the line contains
       */
      val headerSplit: Option[(String, String)] = {
        if (current.startsWith("@import ")) None
        else
          declRemainder.orElse(exprRemainder)
                       .map(r => (r, current.trim.drop(r.length)))
      }

      val (nextIndent, nextLine) = getIndent(next)
      val indent = spaces.length
      val nextIsElse = nextLine.drop(nextIndent).take("@else ".length).trim.startsWith("@else")

      val elseCorrection = if (nextIsElse) 1 else 0

      val delta =
        if (nextIndent > indent && headerSplit.map(_._2).exists(noBraceLine)) 1
        else -stack.takeWhile(_ > nextIndent).length

      val baseLine: String = headerSplit match {
        case None => current
        case Some((before, after)) =>

          val newFirst =
            if (!before.startsWith("@else")) before
            else before.dropRight("@else".length)+ "} else"

          if (delta > 0 && noBraceLine(after)) newFirst + "{" + after
          else if (delta <= 0 && noBraceLine(after) && after.trim != "") newFirst + "{" + after + "}" * (1 - elseCorrection)
          else current


      }

      val closing = "}" * (-delta - elseCorrection)

      val newStack =
        if (delta > 0) nextIndent :: stack
        else stack.drop(-delta)
//      println(stack.toString.padTo(15, ' ') + current.padTo(15, ' ') + indent + "\t" + nextIndent + "\t" + delta + "\t" + headerSplit.toString.padTo(15, ' ') + (baseLine + closing))
      (newStack, text :+ (spaces + baseLine + closing))
    }

    val res = linesOut._2.mkString("\n")
//    println(res)
    res
  }
}
