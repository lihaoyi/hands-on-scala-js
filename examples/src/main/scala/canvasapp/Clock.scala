package canvasapp


import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
@JSExport
object Clock extends{
  @JSExport
  def main(canvas: dom.HTMLCanvasElement) = {
    /*setup*/
    val renderer = canvas.getContext("2d")
                         .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    val gradient = renderer.createLinearGradient(
      canvas.width / 2 - 100, 0, canvas.width/ 2 + 100, 0
    )
    gradient.addColorStop(0,"red")
    gradient.addColorStop(0.5,"green")
    gradient.addColorStop(1,"blue")
    renderer.fillStyle = gradient
    //renderer.fillStyle = "black"

    renderer.textAlign = "center"
    renderer.textBaseline = "middle"

    /*code*/
    def render() = {
      val date = new js.Date()
      renderer.clearRect(
        0, 0, canvas.width, canvas.height
      )

      renderer.font = "75px sans-serif"
      renderer.fillText(
        Seq(
          date.getHours(),
          date.getMinutes(),
          date.getSeconds()
        ).mkString(":"),
        canvas.width / 2,
        canvas.height / 2
      )
    }
    dom.setInterval(render _, 1000)
  }
}