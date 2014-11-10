/*js/shared/test/scala/simple/SimpleTest.scala*/
/*jvm/shared/test/scala/simple/SimpleTest.scala*/
package simple
import utest._
object SimpleTest extends TestSuite{
  val tests = TestSuite{
    'format{
      'nil - assert(Simple.formatTimes(Nil) == "")
      'timeZero - assert(
        Simple.formatTimes(Seq(0)) == "December 31, 1969 4:00:00 PM PST")
     }
  }
}