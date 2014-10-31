package webpage
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
@JSExport
object HelloWorld0 extends{
  @JSExport
  def main(target: dom.HTMLDivElement) = {
    val (animalA, animalB) = ("fox", "dog")
    target.innerHTML = s"""
    <div>
      <h1>Hello World!</h1>
      <p>
        The quick brown <b>$animalA</b>
        jumped over the lazy <i>$animalB</b>
      </p>
    </div>
    """
  }
}