package webpage
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
@JSExport
object HelloWorld0 extends{
  @JSExport
  def main(target: html.Div) ={
    val (f, d) = ("fox", "dog")
    target.innerHTML = s"""
    <div>
      <h1>Hello World!</h1>
      <p>
        The quick brown <b>$f</b>
        jumps over the lazy <i>$d</b>
      </p>
    </div>
    """
  }
}