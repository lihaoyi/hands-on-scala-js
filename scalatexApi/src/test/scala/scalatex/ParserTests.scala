package scalatex


import org.parboiled2._
import torimatomeru.ScalaSyntax

object ParserTests extends utest.TestSuite{
  import Ast._
  import utest._
  def check[T](input: String, parse: ScalatexParser => scala.util.Try[T], expected: T) = {
    val parsed = parse(new ScalatexParser(input))
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

      val str =
        """
          |@omg
          |  @wtf
          |    @bbq
          |      @lol
        """.stripMargin
      new ScalatexParser(str).Body.run()
    }
  }

}


