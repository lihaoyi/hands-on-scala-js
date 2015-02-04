//jvm/src/main/scala/simple/Platform.scala
package simple
import java.text.SimpleDateFormat
import java.util.TimeZone

object Platform{
  def format(ts: Long) = {
    val fmt = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.sss'Z'"
    )
    fmt.setTimeZone(TimeZone.getTimeZone("UTC"))
    fmt.format(new java.util.Date(ts))
  }
}