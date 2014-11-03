//js/src/main/scala/simple/Platform.scala
package simple
import scala.scalajs.js

object Platform extends js.JSApp{
  def format(ts: Long) = {
    new js.Date(ts).toLocaleString()
  }
  def main() = {
    val times = Seq(
      System.currentTimeMillis(),
      System.currentTimeMillis() + 1000
    )
    println("Running on JS! " + 1.0d)
    println(Simple.formatTimes(times))
  }
}