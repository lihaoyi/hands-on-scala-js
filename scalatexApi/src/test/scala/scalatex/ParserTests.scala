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
        val trimmed = wrap(stages.Trim("""
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
        val trimmed = wrap(stages.Trim(
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

        val trimmed = wrap(stages.Trim(
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
              Seq(Text("\n"), Text("\n"), Text("hello world"), Text("\n"))
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
      * - check("{i am a cow}", _.BraceBlock.run(), Block(Seq(Block.Text("i am a cow"))))
      * - check("{i @am a @cow}", _.BraceBlock.run(),
        Block(Seq(
          Block.Text("i "),
          Chain("am",Seq()),
          Block.Text(" a "),
          Chain("cow",Seq())
        ))
      )
    }
    'Chain{
      * - check("@omg.bbq[omg].fff[fff](123)  ", _.ScalaChain.run(),
        Chain("omg",Seq(
          Chain.Prop("bbq"),
          Chain.TypeArgs("[omg]"),
          Chain.Prop("fff"),
          Chain.TypeArgs("[fff]"),
          Chain.Args("(123)")
        ))
      )
      * - check("@omg{bbq}.cow(moo){a @b}\n", _.ScalaChain.run(),
        Chain("omg",Seq(
          Block(Seq(Block.Text("bbq"))),
          Chain.Prop("cow"),
          Chain.Args("(moo)"),
          Block(Seq(Block.Text("a "), Chain("b", Nil)))
        ))
      )
    }
    'ControlFlow{
      'for {
        'for - check(
          "@for(x <- 0 until 3){lol}",
          _.ForLoop.run(),
          For("for(x <- 0 until 3)", Block(Seq(Text("lol"))))
        )
        'forBlock - check(
          """
            |@for(x <- 0 until 3)
            |  lol""".stripMargin,
          _.Body.run(),
          Block(Seq(
            Text("\n"),
            For("for(x <- 0 until 3)", Block(Seq(Text("\n  "), Text("lol"))))
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
            For("for(x <- 0 until 3)", Block(Seq(Text("\n  "), Text("lol"))))
          ))
        )
      }
      'ifElse {
        'if - check(
          "@if(true){lol}",
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("lol"))), None)
        )
        'ifElse - check(
          "@if(true){lol}else{ omg }",
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("lol"))), Some(Block(Seq(Text(" omg ")))))
        )
        'ifBlock - check(
          """@if(true)
            |  omg""".stripMargin,
          _.IfElse.run(),
          IfElse("if(true)", Block(Seq(Text("\n  "), Text("omg"))), None)
        )
        'ifBlockElseBlock - check(
          """@if(true)
            |  omg
            |@else
            |  wtf""".stripMargin,
          _.IfElse.run(),
          IfElse(
            "if(true)",
            Block(Seq(Text("\n  "), Text("omg"))),
            Some(Block(Seq(Text("\n  "), Text("wtf"))))
          )
        )
        'ifElseBlock - check(
          """@if(true){
            |  omg
            |}else
            |  wtf""".stripMargin,
          _.IfElse.run(),
          IfElse(
            "if(true)",
            Block(Seq(Text("\n  "), Text("omg"), Text("\n"))),
            Some(Block(Seq(Text("\n  "), Text("wtf"))))
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
            Text("\n  "),
            Chain("wtf",Seq(Block(Seq(
              Text("\n    "),
              Chain("bbq",Seq(Block(Seq(
                Text("\n      "),
                Chain("lol",Seq())
              ))))
            ))))
          ))))
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
              Text("\n  "),
              Chain("wtf",Seq())
            )
          ))),
          Text("\n"),
          Chain("bbq", Seq())
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
              Text("\n  "),
              Chain("wtf",Seq()),
              Text("\n")
            )
          ))),
          Text("\n"),
          Chain("bbq", Seq())
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
            Args("""("lol", 1, 2)"""),
            Block(Seq(
              Text("\n  "),
              Chain("wtf",Seq())))
          )),
          Text("\n"),
          Text("bbq")
        ))
      )
//      * - check(
//        """
//          |@omg("lol",
//          |1,
//          |       2
//          |    )
//          |  wtf
//          |bbq""".stripMargin,
//        _.Body.run(),
//        Block(Seq(
//          Chain("omg",Seq(
//            Args("(\"lol\",\n1,\n       2\n    )"),
//            Block(Seq(
//              Text("\n  "), Text("wtf")
//            ))
//          )),
//          Text("\n"),
//          Text("bbq")
//        ))
//      )
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
          Chain("{\"lol\" * 3}", Seq()),
          Text("\n"),
          Chain("""{
            |  val omg = "omg"
            |  omg * 2
            |}""".stripMargin,
            Seq()
          )
        ))
      )
    }
  }
}



