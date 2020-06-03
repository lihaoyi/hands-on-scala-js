// library/js/src/main/scala/simple/Platform.scala
package simple

import scala.scalajs.js

private[simple] object Platform {
  def format(ts: Long): String = {
    new js.Date(ts.toDouble)
      .toISOString()
  }
}
