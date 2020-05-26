package webpage

import org.scalajs.dom
import dom.html
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._

@JSExportTopLevel("WebPageInputs")
object Inputs extends{
  @JSExport
  def main(target: html.Div) = {
    val box = input(
      `type`:="text",
      placeholder:="Type here!"
    ).render

    val output = span.render

    box.onkeyup = (e: dom.Event) => {
      output.textContent =
        box.value.toUpperCase
    }

    target.appendChild(
      div(
        h1("Capital Box!"),
        p(
          "Type here and " +
          "have it capitalized!"
        ),
        div(box),
        div(output)
      ).render
    )
  }
}
