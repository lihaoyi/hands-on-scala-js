/*js/src/main/scala/simple/Platform.scala*/
package simple
import scala.scalajs.js

object Platform extends js.JSApp{
  def format(ts: Long) = {
    new js.Date(ts).toLocaleString();
  }
  def main() = {
    println("simple.js")
    val times = Seq(
      System.currentTimeMillis(),
      System.currentTimeMillis() + 1000,
      System.currentTimeMillis() + 2000
    )
    println(Simple.formatTimestamps(times))
  }
}