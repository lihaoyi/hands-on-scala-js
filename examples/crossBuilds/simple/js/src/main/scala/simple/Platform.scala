//js/src/main/scala/simple/Platform.scala
package simple
import scala.scalajs.js

object Platform extends js.JSApp{
  def format(ts: Long) = {
    new js.Date(ts).toISOString()
  }
  def main() = {
    val times = Seq(
      0L,
      1L << 32
    )
    println("Running on JS! " + 1.0d)
    println(Simple.formatTimes(times))
  }
}