import Math._
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

@JSExport
object Example extends{
  @JSExport
  def main(canvas: dom.HTMLCanvasElement) = {

    def clear() = {
      canvas.width = canvas.parentElement.clientWidth
      canvas.height = canvas.parentElement.clientHeight
    }
    clear()

    val renderer =
      canvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

    def h = canvas.height
    def w = canvas.width

    /*example*/
    var x = 0.0
    type Graph = (String, Double => Double)
    val graphs = Seq[Graph](
      ("red", sin),
      ("green", x => 1 - abs(x % 4 - 2)),
      ("blue", x => pow(sin(x/12), 2) * sin(x))
    ).zipWithIndex
    dom.setInterval(() => {
      x = (x + 1) % w; if (x == 0) clear()
      for (((color, f), i) <- graphs) {
        val offset = h / 3 * (i + 0.5)
        val y = f(x / w * 75) * h / 30
        renderer.fillStyle = color
        renderer.fillRect(x, y + offset, 3, 3)
      }
    }, 20)

  }
}