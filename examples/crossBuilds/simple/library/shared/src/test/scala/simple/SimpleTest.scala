// library/shared/src/test/scala/simple/SimpleTest.scala

package simple
import utest._
object SimpleTest extends TestSuite{
  val tests = TestSuite{
    test("format") {
      test("nil") {
        assert(Simple.formatTimes(Nil) == Nil)
      }
      test("timeZero") {
        val timestamps = Seq(0L, 1L << 32)
        val expected = Seq(
          "1970-01-01T00:00:00",
          "1970-02-19T17:02:47"
        )
        val formatted = Simple.formatTimes(timestamps)
        assert(formatted == expected)
      }
    }
    test("zero") {
      0.0
    }
  }
}
