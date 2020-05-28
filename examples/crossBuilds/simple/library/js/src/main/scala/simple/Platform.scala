// library/js/src/main/scala/simple/Platform.scala
package simple
import scalajs.js

object Platform{
  def format(ts: Long) = {
    new js.Date(ts.toDouble).toISOString()
  }
}
