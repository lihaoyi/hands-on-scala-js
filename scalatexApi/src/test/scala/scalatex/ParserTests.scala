package scalatex


import org.parboiled2._
import torimatomeru.ScalaSyntax

import scalatex.Ast.Block.Text
import scalatex.Ast.Chain.Args

object ParserTests extends utest.TestSuite{
  import Ast._
  import utest._
  def check[T](input: String, parse: ScalatexParser => scala.util.Try[T], expected: T) = {
    val parsed = parse(new ScalatexParser(input))
    println(parsed.get)
    println(expected)
    assert(parsed.get == expected)
  }
  def tests = TestSuite{
    'Test {
      * - check("i am a cow", _.Text.run(), Block.Text("i am a cow"))
      * - check("i am a @cow", _.Text.run(), Block.Text("i am a "))
      * - check("i am a @@cow", _.Text.run(), Block.Text("i am a @cow"))
      * - check("i am a @@@cow", _.Text.run(), Block.Text("i am a @"))
      * - check("i am a @@@@cow", _.Text.run(), Block.Text("i am a @@cow"))

    }
    'Code{
      * - check("@(1 + 1)", _.Code.run(),  Code("(1 + 1)"))
      * - check("@{{1} + (1)}", _.Code.run(), Code("{{1} + (1)}"))
      * - check("@{val x = 1; 1}", _.Code.run(), Code("{val x = 1; 1}"))
      * - check("@{`{}}{()@`}", _.Code.run(), Code("{`{}}{()@`}"))
    }

    'Block{
      * - check("{i am a cow}", _.TBlock.run(), Block(Seq(Block.Text("i am a cow"))))
      * - check("{i @am a @cow}", _.TBlock.run(),
        Block(Seq(
          Block.Text("i "),
          Chain(Code("am"),Seq()),
          Block.Text(" a "),
          Chain(Code("cow"),Seq())
        ))
      )
    }
    'Chain{
      * - check("@omg.bbq[omg].fff[fff](123)", _.ScalaChain.run(),
        Chain(Code("omg"),Seq(Chain.Prop(".bbq[omg]"), Chain.Prop(".fff[fff]"), Chain.Args("(123)")))
      )
      * - check("@omg{bbq}.cow(moo){a @b}", _.ScalaChain.run(),
        Chain(Code("omg"),Seq(
          Block(Seq(Block.Text("bbq"))),
          Chain.Prop(".cow"),
          Chain.Args("(moo)"),
          Block(Seq(Block.Text("a "), Chain(Code("b"), Nil)))
        ))
      )
    }
    'Body{

      * - check(
        """
          |@omg
          |  @wtf
          |    @bbq
          |      @lol""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Chain(Code("omg"),Seq(Block(Seq(
            Chain(Code("wtf"),Seq(Block(Seq(
              Chain(Code("bbq"),Seq(Block(Seq(
                Chain(Code("lol"),Seq(Block(Seq(
                ))))
              ))))
            ))))
          ))))
        ))
      )
      * - check(
        """
          |@omg
          |  @wtf
          |@bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Chain(Code("omg"),Seq(Block(
            Seq(
              Text("\n  "),
              Chain(Code("wtf"),Seq()))
          ))),
          Chain(Code("bbq"),
            Seq(Block(Seq()))
          )
        ))
      )
      * - check(
        """
          |@omg("lol", 1, 2)
          |  @wtf
          |bbq""".stripMargin,
        _.Body.run(),
        Block(Seq(
          Chain(Code("omg"),Seq(
            Args("""("lol", 1, 2)"""),
            Block(Seq(
              Text("\n  "),
              Chain(Code("wtf"),Seq())))
          )),
          Text("\n"),
          Text("bbq")
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
          Chain(Code("omg"),Seq(
            Args("(\"lol\",\n1,\n       2\n    )"),
            Block(Seq(
              Text("\n  "), Text("wtf")
            ))
          )),
          Text("\n"),
          Text("bbq")
        ))
      )

    }
  }

}



