import java.lang.Math._

import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport

@JSExport
object Splash extends{
  @JSExport
  def main(canvas: html.Canvas) = {

    def clear() = {
      canvas.width = canvas.parentElement.clientWidth
      canvas.height = canvas.parentElement.clientHeight
    }
    clear()

    val brush =
      canvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

    def h = canvas.height
    def w = canvas.width

    var x = 0.0
    type Graph = (String, Double => Double)
    val graphs = Seq[Graph](
      ("red", sin),
      ("green", x => abs(x % 4 - 2) - 1),
      ("blue", x => sin(x/12) * sin(x))
    ).zipWithIndex
    dom.window.setInterval(() => {
      x = (x + 1) % w; if (x == 0) clear()
      for (((color, f), i) <- graphs) {
        val offset = h / 3 * (i + 0.5)
        val y = f(x / w * 75) * h / 30
        brush.fillStyle = color
        brush.fillRect(x, y + offset, 3, 3)
      }
    }, 20)
  }
}