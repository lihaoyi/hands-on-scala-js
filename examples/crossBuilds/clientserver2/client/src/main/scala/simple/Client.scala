package simple
import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import autowire._

object Ajaxer extends autowire.Client[String, upickle.Reader, upickle.Writer]{
  override def doCall(req: Request) = {
    dom.extensions.Ajax.post(
      url = "/browser/" + req.path.mkString("/"),
      data = upickle.write(req.args)
    ).map(_.responseText)
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

@JSExport
object Client extends{
  @JSExport
  def main(container: dom.HTMLDivElement) = {
    val inputBox = input.render
    val outputBox = ul.render
    def update() = Ajaxer[Api].list(inputBox.value).call().foreach{ data =>
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