package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object Weather0 extends{
  @JSExport
  def main(target: dom.HTMLDivElement) = {
    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET",
      "http://api.openweathermap.org/" +
      "data/2.5/weather?q=Singapore"
    )
    xhr.onload = (e: dom.Event) => {
      if (xhr.status == 200) {
        target.appendChild(
          pre(xhr.responseText).render
        )
      }
    }
    xhr.send()
  }
}