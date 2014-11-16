package scalatex


import org.parboiled2._
import torimatomeru.ScalaSyntax

import scalatex.stages.{Trim, Parser, Ast}
import scalatex.stages.Ast.Block.{IfElse, For, Text}
import Ast.Chain.Args

object ParserTests extends utest.TestSuite{
  import Ast._
  import utest._
  def check[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
    val parsed = parse(new Parser(input)).get
    assert(parsed == expected)
  }
  def tests = TestSuite{
    'Trim{
      def wrap(s: String) = "|" + s + "|"
      * - {
        val trimmed = wrap(stages.Trim.old("""
            i am cow
              hear me moo
                i weigh twice as much as you
        """))
        val expected = wrap("""
                         |i am cow
                         |  hear me moo
                         |    i weigh twice as much as you
                         |""".stripMargin)
         assert(trimmed == expected)

      }
      * - {
        val trimmed = wrap(stages.Trim.old(
          """
          @{"lol" * 3}
          @{
            val omg = "omg"
            omg * 2
          }
          """
        ))
        val expected = wrap(
          """
            |@{"lol" * 3}
            |@{
            |  val omg = "omg"
            |  omg * 2
            |}
            |""".stripMargin
        )
        assert(trimmed == expected)
      }
      'dropTrailingWhitespace - {

        val trimmed = wrap(stages.Trim.old(
          Seq(
            "  i am a cow   ",
            "    hear me moo    ",
            "   i weigh twice as much as you"
          ).mkString("\n")
        ))
        val expected = wrap(
          Seq(
            "i am a cow",
            "  hear me moo",
            " i weigh twice as much as you"
          ).mkString("\n")
        )
        assert(trimmed == expected)
      }
    }
    'Text {
      * - check("i am a cow", _.Text.run(), Block.Text("i am a cow"))
      * - check("i am a @cow", _.Text.run(), Block.Text("i am a "))
      * - check("i am a @@cow", _.Text.run(), Block.Text("i am a @cow"))
      * - check("i am a @@@cow", _.Text.run(), Block.Text("i am a @"))
      * - check("i am a @@@@cow", _.Text.run(), Block.Text("i am a @@cow"))

    }
    'Code{
      'identifier - check("@gggg  ", _.Code.run(), "gggg")
      'parens - check("@(1 + 1)lolsss\n", _.Code.run(),  "(1 + 1)")
      'curlies - check("@{{1} + (1)}  ", _.Code.run(), "{{1} + (1)}")
      'blocks - check("@{val x = 1; 1} ", _.Code.run(), "{val x = 1; 1}")
      'weirdBackticks - check("@{`{}}{()@`}\n", _.Code.run(), "{`{}}{()@`}")
     }
    'MiscCode{
      'imports{
        * - check("@import math.abs", _.Header.run(), "import math.abs")
        * - check("@import math.{abs, sin}", _.Header.run(), "import math.{abs, sin}")
      }
      'headerblocks{
        check(
          """@import math.abs
            |@import math.sin
            |
            |hello world
            |""".stripMargin,
          _.HeaderBlock.run(),
          Ast.Header(
            "import math.abs\nimport math.sin",
            Ast.Block(
              Seq(Text("\n", 33), Text("\n", 34), Text("hello world", 35), Text("\n", 46)),
              33
            )
          )
        )
      }
      'caseclass{
        check(
          """@case class Foo(i: Int, s: String)
          """.stripMargin,
          _.Header.run(),
          "case class Foo(i: Int, s: String)"
        )
      }

    }
    'Block{
      * - check("{i am a cow}", _.BraceBlock.run(), Block(Seq(Block.Text("i am a cow", 1)), 1))
      * - check("{i @am a @cow}", _.BraceBlock.run(),
        Block(Seq(
          Block.Text("i ", 1),
          Chain("am",Seq(), 3),
          Block.Text(" a ", 6),
          Chain("cow",Seq(), 9)
        ), 1)
      )
    }
    'Chain{
      * - check("@omg.bbq[omg].fff[fff](123)  ", _.ScalaChain.run(),
        Chain("omg",Seq(
          Chain.Prop("bbq", 4),
          Chain.TypeArgs("[omg]", 8),
          Chain.Prop("fff", 13),
          Chain.TypeArgs("[fff]", 17),
          Chain.Args("(123)", 22)
        ))
      )
      * - check("@omg{bbq}.cow(moo){a @b}\n", _.ScalaChain.run(),
        Chain("omg",Seq(
          Block(Seq(Text("bbq", 5)), 5),
          Chain.Prop("cow", 9),
          Chain.Args("(moo)", 13),
          Block(Seq(Text("a ", 19), Chain("b", Nil, 21)), 19)
        ))
      )
    }
    'ControlFlow{
      'for {
        'for - check(
          "@for(x <- 0 until 3){lol}",
          _.ForLoop.run(),
          For("for(x <- 0 until 3)", Block(Seq(Text("lol", 21)), 21))
        )
        'forBlock - check(
          """
            |@for(x <- 0 until 3)
            |  lol""".stripMargin,
          _.Body.run(),
          Block(Seq(
            Text("\n"),
            For(
              "for(x <- 0 until 3)",
              Block(Seq(Text("\n  ", 21), Text("lol", 24)), 21),
              1
            )
          ))
        )
        'forBlockBraces - check(
          """
            |@for(x <- 0 until 3){
            |  lol
            |}""".stripMargin,
          _.Body.run(),
          Block(Seq(
            Text("\n"),
            For(
              "for(x <- 0 until 3)",
              Block(Seq(Text("\n  ", 22), Text("lol", 25), Text("\n", 28)), 22),
              1
            )
          ))
        )
      }
      'ifElse {
        'if - check(
          "@if(true){lol}",
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("lol", 10)), 10), None)
        )
        'ifElse - check(
          "@if(true){lol}else{ omg }",
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("lol", 10)), 10), Some(Block(Seq(Text(" omg ", 19)), 19)))
        )
        'ifBlock - check(
          """
            |@if(true)
            |  omg""".stripMargin,
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("\n  ", 10), Text("omg", 13)), 10), None, 1)
        )
        'ifBlockElseBlock - check(
          """
            |@if(true)
            |  omg
            |@else
            |  wtf""".stripMargin,
          _.IfElse.run(),
          IfElse(
            "if(true)",
            Block(Seq(Text("\n  ", 10), Text("omg", 13)), 10),
            Some(Block(Seq(Text("\n  ", 22), Text("wtf", 25)), 22)),
          1
          )
        )
        'ifBlockElseBraceBlock - check(
          """@if(true){
            |  omg
            |}else{
            |  wtf
            |}""".stripMargin,
          _.IfElse.run(),
          IfElse(
            "if(true)",
            Block(Seq(Text("\n  ", 10), Text("omg", 13), Text("\n", 16)), 10),
            Some(Block(Seq(Text("\n  ", 23), Text("wtf", 26), Text("\n", 29)), 23)),
            0
          )
        )
        'ifBlockElseBraceBlockNested - {
          val res = Parser(Trim.old(
            """
        @p
          @if(true){
            Hello
          }else{
            lols
          }
            """))
          val expected =
            Block(Vector(
              Text("\n"),
              Chain("p",Vector(Block(Vector(
                Text("\n  ", 3),
                IfElse("if(true)",
                  Block(Vector(
                    Text("\n    ", 16), Text("Hello", 21), Text("\n  ", 26)
                  ), 16),
                  Some(Block(Vector(
                    Text("\n    ", 35), Text("lols", 40), Text("\n  ", 44)
                  ), 35)),
                  6
                )), 3)), 1),
              Text("\n", 48)
            ))
          assert(res == expected)
        }
        'ifElseBlock - check(
          """@if(true){
            |  omg
            |}else
            |  wtf""".stripMargin,
          _.IfElse.run(),
          IfElse(
            "if(true)",
            Block(Seq(Text("\n  ", 10), Text("omg", 13), Text("\n", 16)), 10),
            Some(Block(Seq(Text("\n  ", 22), Text("wtf", 25)), 22))
          )
        )
      }

    }
    'Body{
      'indents - check(
        """
          |@omg
          |  @wtf
          |    @bbq
          |      @lol""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Text("\n"),
          Chain("omg",Seq(Block(Seq(
            Text("\n  ", 5),
            Chain("wtf",Seq(Block(Seq(
              Text("\n    ", 7),
              Chain("bbq",Seq(Block(Seq(
                Text("\n      ", 9),
                Chain("lol",Seq(), 16)
              ), 9)), 12)
            ), 7)), 8)
          ), 5)), 1)
        ))
      )
      'dedents - check(
        """
          |@omg
          |  @wtf
          |@bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Text("\n"),
          Chain("omg",Seq(Block(
            Seq(
              Text("\n  ", 5),
              Chain("wtf",Seq(), 8)
            ),
            5
          )), 1),
          Text("\n", 12),
          Chain("bbq", Seq(), 13)
        ))
      )
      'braces - check(
        """
          |@omg{
          |  @wtf
          |}
          |@bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Text("\n"),
          Chain("omg",Seq(Block(
            Seq(
              Text("\n  ", 6),
              Chain("wtf",Seq(), 9),
              Text("\n", 13)
            ),
            6
          )), 1),
          Text("\n", 15),
          Chain("bbq", Seq(), 16)
        ))
      )
      'dedentText - check(
        """
          |@omg("lol", 1, 2)
          |  @wtf
          |bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Text("\n"),
          Chain("omg",Seq(
            Args("""("lol", 1, 2)""", 5),
            Block(Seq(
              Text("\n  ", 18),
              Chain("wtf",Seq(), 21)
            ), 18)
          ), 1),
          Text("\n", 25),
          Text("bbq", 26)
        ))
      )
      * - check(
        """
          |@omg("lol",
          |1,
          |       2
          |    )
          |  wtf
          |bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Chain("omg",Seq(
            Args("(\"lol\",\n1,\n       2\n    )"),
            Block(Seq(
              Text("\n  "), Text("wtf")
            ))
          )),
          Text("\n"),
          Text("bbq")
        ))
      )
      'codeBlocks - check(
        """
          |@{"lol" * 3}
          |@{
          |  val omg = "omg"
          |  omg * 2
          |}""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Text("\n"),
          Chain("{\"lol\" * 3}", Seq(), 1),
          Text("\n", 13),
          Chain("""{
            |  val omg = "omg"
            |  omg * 2
            |}""".stripMargin,
            Seq(),
            14
          )
        ))
      )
    }
//    'Test{
//      check(
//        "@{() => ()}",
//        _.Code.run(),
//        ""
//      )
//    }
  }
}



