package webpage

import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object Inputs extends{
  @JSExport
  def main(target: dom.HTMLDivElement) = {
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