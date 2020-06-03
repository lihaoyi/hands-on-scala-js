package simple

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.Ajax
import scalatags.JsDom.all._

@JSExportTopLevel("SimpleClient")
object Client {
  @JSExport
  def main(container: html.Div): Unit = {
    val inputBox = input.render
    val outputBox = ul.render
    def update(): Unit = {
      Ajax.post("/ajax/list", inputBox.value).foreach{ xhr =>
        val data = upickle.default.read[Seq[FileData]](
          ujson.read(xhr.responseText))
        outputBox.innerHTML = ""
        for(FileData(name, size) <- data){
          outputBox.appendChild(
            li(
              b(name), " - ", size, " bytes"
            ).render
          )
        }
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
