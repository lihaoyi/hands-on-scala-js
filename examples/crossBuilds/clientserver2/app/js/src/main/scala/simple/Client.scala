package simple

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.Ajax
import scalatags.JsDom.all._
import upickle.default._
import autowire._

object Ajaxer extends autowire.Client[String, Reader, Writer]{
  override def doCall(req: Request): Future[String] = {
    Ajax.post(
      url = "/ajax/" + req.path.mkString("/"),
      data = write(req.args)
    ).map(_.responseText)
  }

  def read[Result: Reader](p: String): Result = upickle.default.read[Result](p)
  def write[Result: Writer](r: Result): String = upickle.default.write(r)
}

@JSExportTopLevel("SimpleClient")
object Client {
  @JSExport
  def main(container: html.Div) = {
    val inputBox = input.render
    val outputBox = ul.render
    def update(): Unit = {
      Ajaxer[Api].list(inputBox.value).call().foreach{ data =>
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
