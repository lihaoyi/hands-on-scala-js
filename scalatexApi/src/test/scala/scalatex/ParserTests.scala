package scalatex
import utest._
import scalatex.stages.{TwistNodes, Parser}

/**
 * Created by haoyi on 8/2/14.
 */
object ParserTests extends TestSuite{
  val WN = TwistNodes
  val WP = Parser
  def check[T](s: String, f: Parser => T, expected: Option[T]){
    val parsed = WP.parse(s, f).toOption
    assert(parsed == expected)
  }
  val tests = TestSuite{
//    'chainedExpressions {
//      check("", _.expression(), None)
//      check("asd", _.expression(), None)
//      check("@asd", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd"))))
//      ))
//
//      check("@asd{", _.expression(), None)
//      check("@asd(", _.expression(), None)
//      check("@asd()", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd()"))))
//      ))
//      check("@asd(ggnore)", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd(ggnore)"))))
//      ))
//      check("@asd.wtf(ggnore).bbq.lol", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd"), WN.Simple(".wtf(ggnore).bbq.lol"))))
//      ))
//      check("@asd{}", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd"), WN.Block("", None, Seq()))))
//      ))
//      check("@asd{lol}", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(WN.Simple("asd"), WN.Block("", None, Seq(WN.Plain("lol"))))))
//      ))
//      check("@asd{lol}.wtf('l'){gg}", _.expression(), Some(
//        WN.Display(WN.ScalaExp(Seq(
//          WN.Simple("asd"),
//          WN.Block("", None, Seq(WN.Plain("lol"))),
//          WN.Simple(".wtf('l')"),
//          WN.Block("", None, Seq(WN.Plain("gg")))
//        )))
//      ))
//    }
  }
}
