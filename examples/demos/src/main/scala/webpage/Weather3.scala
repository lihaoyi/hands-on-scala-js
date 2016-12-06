package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import dom.html

@JSExport
object Weather3 extends{
  @JSExport
  def main(target: html.Div) = {
    import dom.ext._
    import scala.scalajs
    .concurrent
    .JSExecutionContext
    .Implicits
    .runNow

    /**
      * This is the API key for "Hands-on Scala.js".
      * If you would like to integrate weather data into your
      * own development, please register for a free API key at:
      * http://www.openweathermap.org/appid
      */
    val weatherkey="4ef01dbbb326222af5ec69053f824bde"

    val url =
      "http://api.openweathermap.org/" +
        "data/2.5/weather?q=Singapore&appid=" +
        weatherkey

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