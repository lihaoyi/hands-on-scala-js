package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._
import dom.html
import WeatherAPIKey.APIKey

@JSExportTopLevel("WebPageWeather1")
object Weather1 {
  @JSExport
  def main(target: html.Div) = {
    import dom.ext._
    import scala.concurrent
                .ExecutionContext
                .Implicits
                .global

    val url =
      "https://api.openweathermap.org/" +
      "data/2.5/weather?q=Singapore" +
      s"&APPID=$APIKey"

    Ajax.get(url).foreach { xhr =>
      target.appendChild(
        pre(xhr.responseText).render
      )
    }
  }
}
