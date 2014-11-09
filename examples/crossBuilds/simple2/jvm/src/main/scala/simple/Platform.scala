//jvm/src/main/scala/simple/Platform.scala
package simple
import java.text.SimpleDateFormat

object Platform{
  def format(ts: Long) = {
    val fmt =
      "MMMM d, yyyy h:mm:ss aaa z"
    new SimpleDateFormat(fmt).format(
      new java.util.Date(ts)
    )
  }
}