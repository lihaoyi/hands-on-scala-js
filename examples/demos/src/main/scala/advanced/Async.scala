package advanced

import org.scalajs.dom
import concurrent._
import async.Async._
import scalajs.js.annotation.JSExport
import scalajs.concurrent.JSExecutionContext.Implicits.queue

@JSExport
object Async {
  def init(canvas: dom.HTMLCanvasElement) = {
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.style.backgroundColor = "#f8f8f8"
    canvas.height = canvas.parentElement.clientHeight
    canvas.width = canvas.parentElement.clientWidth

    renderer.lineWidth = 5
    renderer.strokeStyle = "red"
    renderer.fillStyle = "cyan"
    renderer
  }
  @JSExport
  def main(canvas: dom.HTMLCanvasElement) = {
    val renderer = init(canvas)
    // async
    def rect = canvas.getBoundingClientRect()

    type ME = dom.MouseEvent
    val mousemove =
      new Channel[ME](canvas.onmousemove = _)
    val mouseup =
      new Channel[ME](canvas.onmouseup = _)
    val mousedown =
      new Channel[ME](canvas.onmousedown = _)

    async{
      while(true){
        val start = await(mousedown())
        renderer.beginPath()
        renderer.moveTo(
          start.clientX - rect.left,
          start.clientY - rect.top
        )

        var res = await(mousemove | mouseup)
        while(res.`type` == "mousemove"){
          renderer.lineTo(
            res.clientX - rect.left,
            res.clientY - rect.top
          )
          renderer.stroke()
          res = await(mousemove | mouseup)
        }

        renderer.fill()
        await(mouseup())
        renderer.clearRect(0, 0, 1000, 1000)
      }
    }
  }
  @JSExport
  def main0(canvas: dom.HTMLCanvasElement) = {
    val renderer = init(canvas)
    // traditional
    def rect = canvas.getBoundingClientRect()

    var dragState = 0

    canvas.onmousemove ={(e: dom.MouseEvent) =>
      if (dragState == 1) {
        renderer.lineTo(
          e.clientX - rect.left,
          e.clientY - rect.top
        )
        renderer.stroke()
      }
    }
    canvas.onmouseup = {(e: dom.MouseEvent) =>
      if(dragState == 1) {
        renderer.fill()
        dragState = 2
      }else if (dragState == 2){
        renderer.clearRect(0, 0, 1000, 1000)
        dragState = 0
      }
    }
    canvas.onmousedown ={(e: dom.MouseEvent) =>
      if (dragState == 0) {
        dragState = 1
        renderer.beginPath()
        renderer.moveTo(
          e.clientX - rect.left,
          e.clientY - rect.top
        )
      }
    }
  }
}

class Channel[T](init: (T => Unit) => Unit){
  init(update)
  private[this] var value: Promise[T] = null
  def apply(): Future[T] = {
    value = Promise[T]()
    value.future
  }
  def update(t: T): Unit = {
    if (value != null && !value.isCompleted) value.success(t)
  }
  def |(other: Channel[T]): Future[T] = {
    val p = Promise[T]()
    for{
      f <- Seq(other(), this())
      t <- f
    } p.trySuccess(t)
    p.future
  }
}