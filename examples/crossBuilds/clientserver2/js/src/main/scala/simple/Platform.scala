//js/src/main/scala/simple/Platform.scala
package simple

import scala.scalajs.js
import org.scalajs.dom
import dom.extensions.Ajax
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
@JSExport
object Platform extends{
  def format(ts: Long) = {
    new js.Date(ts).toLocaleString()
  }
  @JSExport
  def main(container: dom.HTMLDivElement) = {
    container.appendChild(
      div(
        h1("Hello from Scala.js!"),
        p(Simple.formatTimes(Seq(0, 1 << 30)))
      ).render
    )
    val payload = upickle.write(Seq(0L, 1L << 30))
    Ajax.post("/formatDates", payload).foreach{ xhr =>
      container.appendChild(
        div(
          h1("Hello from Ajax!"),
          p(xhr.responseText)
        ).render
      )
    }

  }
}