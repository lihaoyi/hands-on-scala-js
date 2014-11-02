/*jvm/src/main/scala/simple/Platform.scala*/
package simple
import java.text.SimpleDateFormat

object Platform{
  def format(ts: Long) = {
    // http://docs.oracle.com/javase/7/
    // docs/api/java/text/SimpleDateFormat.html
    val fmt = "MMMM d, yyyy h:mm:ss aaa z"
    new SimpleDateFormat(fmt).format(
      new java.util.Date(ts)
    )
  }
  def main(args: Array[String]) = {
    println("simple.jvm")
    val times = Seq(
      System.currentTimeMillis(),
      System.currentTimeMillis() + 1000,
      System.currentTimeMillis() + 2000
    )
    println(Simple.formatTimestamps(times))
  }
}