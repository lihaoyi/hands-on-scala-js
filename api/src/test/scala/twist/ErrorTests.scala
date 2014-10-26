package twist

import utest._
import twist.stages._
import scalatags.Text.all._
import twist.Internals.{DebugFailure, twDebug}

/**
* Created by haoyi on 7/14/14.
*/
object ErrorTests extends TestSuite{
  def check(x: => Unit, expectedMsg: String, expectedError: String) = {
    val DebugFailure(msg, pos) = intercept[DebugFailure](x)
    def format(str: String) = {
      val whitespace = " \t\n".toSet
      "\n" + str.dropWhile(_ == '\n')
                .reverse
                .dropWhile(whitespace.contains)
                .reverse
    }
    // Format these guys nicely to normalize them and make them
    // display nicely in the assert error message if it blows up
    val formattedPos = format(pos)
    val formattedExpectedPos = format(expectedError)

    assert(msg.contains(expectedMsg))
    assert(formattedPos == formattedExpectedPos)

  }
  val tests = TestSuite{


    'simple - check(
      twDebug("omg @notInScope lol"),
      """not found: value notInScope""",
      """
      twDebug("omg @notInScope lol"),
                   ^
      """
    )

    'chained{
      'properties {
        * - check(
          twDebug("omg @math.lol lol"),
          """object lol is not a member of package math""",
          """
          twDebug("omg @math.lol lol"),
                            ^
          """
        )

        * - check(
          twDebug("omg @math.E.lol lol"),
          """value lol is not a member of Double""",
          """
          twDebug("omg @math.E.lol lol"),
                              ^
          """
        )
        * - check(
          twDebug("omg @_root_.scala.math.lol lol"),
          """object lol is not a member of package math""",
          """
          twDebug("omg @_root_.scala.math.lol lol"),
                                         ^
          """
        )
        * - check(
          twDebug("omg @_root_.scala.gg.lol lol"),
          """object gg is not a member of package scala""",
          """
          twDebug("omg @_root_.scala.gg.lol lol"),
                                    ^
          """
        )
        * - check(
          twDebug("omg @_root_.ggnore.math.lol lol"),
          """object ggnore is not a member of package <root>""",
          """
          twDebug("omg @_root_.ggnore.math.lol lol"),
                              ^
          """
        )
      }
      'calls{
        * - check(
          twDebug("@scala.QQ.abs(-10).tdo(10).sum.z"),
          """object QQ is not a member of package scala""",
          """
          twDebug("@scala.QQ.abs(-10).tdo(10).sum.z"),
                         ^
          """
        )
        * - check(
          twDebug("@scala.math.abs(-10).tdo(10).sum.z"),
          "value tdo is not a member of Int",
          """
          twDebug("@scala.math.abs(-10).tdo(10).sum.z"),
                                       ^
          """
        )
        * - check(
          twDebug("@scala.math.abs(-10).to(10).sum.z"),
          "value z is not a member of Int",
          """
          twDebug("@scala.math.abs(-10).to(10).sum.z"),
                                                  ^
          """
        )
        * - check(
          twDebug("@scala.math.abs(-10).to(10).sum.z()"),
          "value z is not a member of Int",
          """
          twDebug("@scala.math.abs(-10).to(10).sum.z()"),
                                                  ^
          """
        )
        * - check(
          twDebug("@scala.math.abs(-10).cow.sum.z"),
          "value cow is not a member of Int",
          """
          twDebug("@scala.math.abs(-10).cow.sum.z"),
                                       ^
          """
        )
        * - check(
          twDebug("@scala.smath.abs.cow.sum.z"),
          "object smath is not a member of package scala",
          """
          twDebug("@scala.smath.abs.cow.sum.z"),
                         ^
          """
        )
        * - check(
          twDebug("""
            I am cow hear me moo
            @scala.math.abs(-10).tdo(10).sum.z
            I weigh twice as much as you
          """),
          "value tdo is not a member of Int",
          """
            @scala.math.abs(-10).tdo(10).sum.z
                                ^
          """
        )
      }
      'callContents{
        * - check(
          twDebug("@scala.math.abs((1, 2).wtf)"),
          "value wtf is not a member of (Int, Int)",
          """
          twDebug("@scala.math.abs((1, 2).wtf)"),
                                         ^
          """
        )

        * - check(
          twDebug("@scala.math.abs((1, 2).swap._1.toString().map(_.toString.wtf))"),
          "value wtf is not a member of String",
          """
          twDebug("@scala.math.abs((1, 2).swap._1.toString().map(_.toString.wtf))"),
                                                                           ^
          """
        )
      }
    }
    'ifElse{
      'oneLine {
        * - check(
          twDebug("@if(math > 10){ 1 }else{ 2 }"),
          "object > is not a member of package math",
          """
          twDebug("@if(math > 10){ 1 }else{ 2 }"),
                            ^
          """
        )
        * - check(
          twDebug("@if(true){ (@math.pow(10)) * 10  }else{ 2 }"),
          "Unspecified value parameter y",
          """
          twDebug("@if(true){ (@math.pow(10)) * 10  }else{ 2 }"),
                                       ^
          """
        )
        * - check(
          twDebug("@if(true){ * 10  }else{ @math.sin(3, 4, 5) }"),
          "too many arguments for method sin: (x: Double)Double",
          """
          twDebug("@if(true){ * 10  }else{ @math.sin(3, 4, 5) }"),
                                                   ^
          """
        )
      }
      'multiLine{
        * - check(
          twDebug("""
            Ho Ho Ho

            @if(math != 10)
              I am a cow
            @else
              You are a cow
            GG
          """),
          "object != is not a member of package math",
          """
            @if(math != 10)
                     ^
          """
        )
        * - check(
          twDebug("""
            Ho Ho Ho

            @if(4 != 10)
              I am a cow @math.lols
            @else
              You are a cow
            GG
          """),
          "object lols is not a member of package math",
          """
              I am a cow @math.lols
                               ^
          """
        )
        * - check(
          twDebug("""
            Ho Ho Ho

            @if(12 != 10)
              I am a cow
            @else
              @math.E.toString.gog(1)
            GG
          """),
          "value gog is not a member of String",
          """
              @math.E.toString.gog(1)
                                 ^
          """
        )
      }
    }
    'forLoop{
      'oneLine{
        'header - check(
          twDebug("omg @for(x <- (0 + 1 + 2) omglolol (10 + 11 + 2)){ hello }"),
          """value omglolol is not a member of Int""",
          """
          twDebug("omg @for(x <- (0 + 1 + 2) omglolol (10 + 11 + 2)){ hello }"),
                                              ^
          """
        )

        'body - check(
          twDebug("omg @for(x <- 0 until 10){ @((x, 2) + (1, 2)) }"),
          """too many arguments for method +""",
          """
          twDebug("omg @for(x <- 0 until 10){ @((x, 2) + (1, 2)) }"),
                                                       ^
          """
        )
      }
      'multiLine{
        'body - check(
          twDebug("""
            omg
            @for(x <- 0 until 10)
              I am cow hear me moo
              I weigh twice as much as @x.kkk
          """),
          """value kkk is not a member of Int""",
          """
              I weigh twice as much as @x.kkk
                                          ^
          """
        )
      }
    }
    'multiLine{
      'missingVar - check(
        twDebug("""
        omg @notInScope lol
        """),
        """not found: value notInScope""",
        """
        omg @notInScope lol
            ^
        """
      )
      'wrongType - check(
        twDebug("""
        omg @{() => ()} lol
        """),
        """type mismatch""",
        """
        omg @{() => ()} lol
                 ^
        """
      )

      'bigExpression - check(
        twDebug("""
          @{
            val x = 1 + 2
            val y = new Object()
            val z = y * x
            x
          }
        """),
        "value * is not a member of Object",
        """
            val z = y * x
                      ^
        """
      )
    }
  }
}
