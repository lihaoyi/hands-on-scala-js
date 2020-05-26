package webpage

import org.scalajs.dom
import org.scalajs.dom.{Node, Element}
import scalajs.js
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._
import dom.html

@JSExportTopLevel("WebPageWeather2")
object Weather2 {
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
