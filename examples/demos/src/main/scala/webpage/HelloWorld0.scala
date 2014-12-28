package webpage
import org.scalajs.dom
import scalajs.js.annotation.JSExport
@JSExport
object HelloWorld0 extends{
  @JSExport
  def main(target: dom.HTMLDivElement) ={
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