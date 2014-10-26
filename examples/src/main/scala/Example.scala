import Math._
import org.scalajs.dom

object Example extends scalajs.js.JSApp{
  def main() = {
    val canvas =
      dom.document
         .getElementById("example-canvas")
         .asInstanceOf[dom.HTMLCanvasElement]

    val renderer =
      canvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

    val (h, w) = (canvas.height, canvas.width)
    var x = 0.0
    val graphs = Seq[(String, Double => Double)](
      ("red", sin),
      ("green", x => 2 - abs(x % 8 - 4)),
      ("blue", x => 3 * pow(sin(x / 12), 2) * sin(x))
    ).zipWithIndex
    dom.setInterval(() => {
      x = (x + 1) % w
      if (x == 0) renderer.clearRect(0, 0, w, h)
      else for (((color, func), i) <- graphs) {
        val y = func(x/w * 75) * h/40 + h/3 * (i+0.5)
        renderer.fillStyle = color
        renderer.fillRect(x, y, 3, 3)
      }
    }, 10)

  }
}