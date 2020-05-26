package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._
import dom.html

@JSExportTopLevel("WebPageWeather3")
object Weather3 {
  @JSExport
  def main(target: html.Div) = {
    import dom.ext._
    import scala.scalajs
    .concurrent
    .JSExecutionContext
    .Implicits
    .runNow

    val url =
      "http://api.openweathermap.org/" +
        "data/2.5/weather?q=Singapore"

    Ajax.get(url).onSuccess{ case xhr =>
      if (xhr.status == 200) {
        val json = js.JSON.parse(
          xhr.responseText
        )
        val name = json.name.toString
        val weather = json.weather
                          .pop()
                          .main
                          .toString

        def celsius(kelvins: js.Dynamic) = {
          kelvins.asInstanceOf[Double] - 273.15
        }.toInt
        val min = celsius(json.main.temp_min)
        val max = celsius(json.main.temp_max)
        val humid = json.main.humidity.toString
        target.appendChild(
          div(
            b("Weather in Singapore:"),
            ul(
              li(b("Country "), name),
              li(b("Weather "), weather),
              li(b("Temp "), min, " - ", max),
              li(b("Humidity "), humid, "%")
            )
          ).render
        )
      }
    }
  }
}
