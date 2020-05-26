package simple

import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom
import dom.html
import dom.ext.Ajax
import scalajs.js.annotation._

@JSExportTopLevel("SimpleClient")
object Client {
  @JSExport
  def main(container: html.Div) = {
    val inputBox = input.render
    val outputBox = ul.render
    def update() = Ajax.post("/ajax/list", inputBox.value).foreach{ xhr =>
      val data = upickle.default.read[Seq[FileData]](xhr.responseText)
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
