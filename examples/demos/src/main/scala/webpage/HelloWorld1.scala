package webpage
import org.scalajs.dom
import dom.html
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._

@JSExportTopLevel("WebPageHelloWorld1")
object HelloWorld1 {
  @JSExport
  def main(target: html.Div) = {
    val (animalA, animalB) = ("fox", "dog")
    target.appendChild(
      div(
        h1("Hello World!"),
        p(
          "The quick brown ", b(animalA),
          " jumps over the lazy ",
          i(animalB), "."
        )
      ).render
    )
  }
}
