package webpage
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
@JSExport
object HelloWorld1 extends{
  @JSExport
  def main(target: dom.HTMLDivElement) = {
    val (animalA, animalB) = ("fox", "dog")
    target.appendChild(
      div(
        h1("Hello World!"),
        p("The quick brown ", b(animalA),
          " jumped over the lazy ", i(animalB), ".")
      ).render
    )
  }
}