package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._
import dom.html

@JSExportTopLevel("WebPageWeather0")
object Weather0 {
  @JSExport
  def main(target: html.Div) = {
    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET",
      "https://api.openweathermap.org/" +
      "data/2.5/weather?q=Singapore" +
      s"&APPID=${WeatherAPIKey.APIKey}"
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
