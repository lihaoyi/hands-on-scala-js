package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import dom.html

@JSExport
object Weather2 extends{
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
      target.appendChild(
        pre(
          js.JSON.stringify(
            js.JSON.parse(xhr.responseText),
            space=4
          )
        ).render
      )
    }
  }
}