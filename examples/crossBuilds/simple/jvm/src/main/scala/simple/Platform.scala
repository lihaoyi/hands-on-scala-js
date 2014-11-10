//jvm/src/main/scala/simple/Platform.scala
package simple
import java.text.SimpleDateFormat
import java.util.{TimeZone, Locale}

object Platform{
  def format(ts: Long) = {
    val fmt = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.sss'Z'"
    )
    fmt.setTimeZone(TimeZone.getTimeZone("UTC"))
    fmt.format(new java.util.Date(ts))
  }
  def main(args: Array[String]) = {
    val times = Seq(
      0L,
      1L << 32
    )
    println("Running on JVM! " + 1.0d)
    println(Simple.formatTimes(times))
  }
}