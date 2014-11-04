package scalatex


import org.parboiled2._
import torimatomeru.ScalaSyntax

import scalatex.stages.{Parser, Ast}
import Ast.Block.Text
import Ast.Chain.Args

object ParserTests extends utest.TestSuite{
  import Ast._
  import utest._
  def check[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
    val parsed = parse(new Parser(input))
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
      * - check("@(1 + 1)lolsss\n", _.Code.run(),  "(1 + 1)")
      * - check("@{{1} + (1)}  ", _.Code.run(), "{{1} + (1)}")
      * - check("@{val x = 1; 1} ", _.Code.run(), "{val x = 1; 1}")
      * - check("@{`{}}{()@`}\n", _.Code.run(), "{`{}}{()@`}")
      * - check("@gggg  ", _.Code.run(), "gggg")
     }

    'Block{
      * - check("{i am a cow}", _.TBlock.run(), Block(Seq(Block.Text("i am a cow"))))
      * - check("{i @am a @cow}", _.TBlock.run(),
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
    'Body{
      * - check(
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
      * - check(
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
              Chain("wtf",Seq()))
          ))),
          Text("\n"),
          Chain("bbq", Seq())
        ))
      )
      * - check(
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
      * - check(
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



