package webpage

import org.scalajs.dom
import dom.html
import scala.scalajs.js.annotation._

@JSExportTopLevel("WebPageHelloWorld0")
object HelloWorld0 {
  @JSExport
  def main(target: html.Div) ={
    val (f, d) = ("fox", "dog")
    target.innerHTML = s"""
    <div>
      <h1>Hello World!</h1>
      <p>
        The quick brown <b>$f</b>
        jumps over the lazy <i>$d</i>
      </p>
    </div>
    """
  }
}
