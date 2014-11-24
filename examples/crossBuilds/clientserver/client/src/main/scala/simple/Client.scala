package simple

import scalatags.JsDom.all._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import org.scalajs.dom
import dom.extensions.Ajax
import scala.scalajs.js.annotation.JSExport

@JSExport
object Client extends{
  @JSExport
  def main(container: dom.HTMLDivElement) = {
    val inputBox = input.render
    val outputBox = ul.render
    def update() = Ajax.post("/ajax/list", inputBox.value).foreach{ xhr =>
      val data = upickle.read[Seq[FileData]](xhr.responseText)
      outputBox.innerHTML = ""
      for(FileData(name, size) <- data){
        outputBox.appendChild(
          li(
            b(name), " - ", size, " bytes"
          ).render
        )
      }
    }
    inputBox.onkeyup = (e: dom.Event) => update()
    update()
    container.appendChild(
      div(
        h1("File Search"),
        inputBox,
        outputBox
      ).render
    )
  }
}