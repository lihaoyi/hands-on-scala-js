package webpage

import org.scalajs.dom
import dom.ext._
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scalajs.js
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import dom.html

@JSExport
object WeatherSearch extends{
  @JSExport
  def main(target: html.Div) = {

    lazy val box = input(
      `type`:="text",
      placeholder:="Type here!"
    ).render

    lazy val output = div(
      height:="400px",
      overflowY:="scroll"
    ).render

    box.onkeyup = (e: dom.Event) => {
      output.innerHTML = "Loading..."
      fetchWeather(box.value)
    }

    target.appendChild(
      div(
        h1("Weather Search"),
        p(
          "Enter the name of a city to pull the ",
          "latest weather data from api.openweathermap.com!"
        ),
        p(box),
        hr, output, hr
      ).render
    )

    def fetchWeather(query: String) = {

      /**
        * This is the API key for "Hands-on Scala.js".
        * If you would like to integrate weather data into your
        * own development, please register for a free API key at:
        * http://www.openweathermap.org/appid
        */
      val weatherkey="4ef01dbbb326222af5ec69053f824bde"

      val searchUrl =
        "http://api.openweathermap.org/data/" +
        "2.5/find?type=like&mode=json&q=" +
        query + "&appid=" + weatherkey

      for{
        xhr <- Ajax.get(searchUrl)
        if query == box.value
      } js.JSON.parse(xhr.responseText).list match{
        case jsonlist: js.Array[js.Dynamic] =>
          output.innerHTML = ""
          showResults(jsonlist, query)
        case _ =>
          output.innerHTML = "No Results"
      }
    }

    def showResults(jsonlist: js.Array[js.Dynamic], query: String) = {
      for (json <- jsonlist) {
        val name = json.name.toString
        val country = json.sys.country.toString
        val weather = json.weather.pop().main.toString

        def celsius(kelvins: js.Dynamic) = {
          kelvins.asInstanceOf[Double] - 273.15
        }.toInt

        val min = celsius(json.main.temp_min)
        val max = celsius(json.main.temp_max)
        val humid = json.main.humidity.toString
        val (first, last) = name.splitAt(query.length)
        output.appendChild(
          div(
            b(span(first, backgroundColor:="yellow"), last, ", ", country),
            ul(
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