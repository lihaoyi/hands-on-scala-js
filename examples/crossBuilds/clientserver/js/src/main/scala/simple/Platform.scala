//js/src/main/scala/simple/Platform.scala
package simple

import org.scalajs.dom.XMLHttpRequest

import scala.scalajs.js
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

@JSExport
object Platform extends{
  def format(ts: Long) = {
    new js.Date(ts).toLocaleString()
  }
  @JSExport
  def main(container: dom.HTMLDivElement) = {
    container.innerHTML +=
      "<h1>Hello from Scala.js!</h1>" +
      s"<p>${Simple.formatTimes(Seq(0, 1 << 30))}</p>"

    val xhr = new XMLHttpRequest()
    xhr.open("POST", "/formatDates")
    xhr.onload = (e: dom.Event) => {
      container.innerHTML +=
        "<h1>Hello from Ajax!</h1>" +
        s"<p>${xhr.responseText}</p>"
    }
    xhr.send(Seq(0, 1 << 30).mkString(","))
  }
}