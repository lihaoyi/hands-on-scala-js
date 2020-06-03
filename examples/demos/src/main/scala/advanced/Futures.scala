package advanced

import org.scalajs.dom
import dom.html
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.ext.{Ajax, KeyCode}
import scala.collection.mutable
import scala.concurrent.Future
import scalajs.js
import scalatags.JsDom.all._
import scala.scalajs.js.annotation._
import scala.concurrent.ExecutionContext.Implicits.global
import webpage.WeatherAPIKey.APIKey

@JSExportTopLevel("AdvancedFutures")
object Futures {
  def main(container: html.Div,
           handle: (Seq[String], html.Div) => Unit): Unit = {
    val myInput = input(value:="London,Singapore,Berlin,New York").render
    val output = div.render
    myInput.onkeyup = (e: dom.KeyboardEvent) => {
      if (e.keyCode == KeyCode.Enter){
        handle(myInput.value.split(',').toSeq, output)
      }
    }
    container.appendChild(
      div(
        i("Press Enter in the box to fetch temperatures "),
        myInput,
        output
      ).render
    )
  }

  def urlFor(name: String): String = {
    "https://api.openweathermap.org/data/" +
    s"2.5/find?mode=json&q=$name" +
    s"&APPID=$APIKey"
  }

  def parseTemp(text: String): Double = {
    val data = js.JSON.parse(text)
    val kelvins = data.list
                      .pop()
                      .main
                      .temp
                      .asInstanceOf[Double]
    kelvins - 273.15
  }

  def formatResults(output: html.Element,
                    results: Seq[(String, Double)]): dom.Node = {
    output.innerHTML = ""
    output.appendChild(ul(
      for((name, temp) <- results) yield li(
        b(name), " - ", temp.toInt, "C"
      )
    ).render)
  }

  @JSExport
  def main0(container: html.Div): Unit = {
    def handle0(names: Seq[String], output: html.Div): Unit = {
      val results = mutable.ListBuffer.empty[(String, Double)]
      for(name <- names){
        val xhr = new XMLHttpRequest
        xhr.open("GET", urlFor(name))
        xhr.onload = (e: dom.Event) => {
          val temp = parseTemp(xhr.responseText)
          results.append((name, temp))
          if (results.length == names.length){
            formatResults(output, results.toList)
          }
        }
        xhr.send()
      }
    }
    main(container, handle0)
  }

  @JSExport
  def main1(container: html.Div): Unit = {
    def handle1(names: Seq[String], output: html.Div): Unit = {
      val results = mutable.ListBuffer.empty[(String, Double)]
      for{
        name <- names
        xhr <- Ajax.get(urlFor(name))
      } {
        val temp = parseTemp(xhr.responseText)
        results.append((name, temp))
        if (results.length == names.length){
          formatResults(output, results.toList)
        }
      }
    }
    main(container, handle1)
  }

  @JSExport
  def main2(container: html.Div): Unit = {
    def handle2(names: Seq[String], output: html.Div): Unit = {
      val futures = for(name <- names) yield{
        Ajax.get(urlFor(name)).map( xhr =>
          (name, parseTemp(xhr.responseText))
        )
      }

      for(results <- Future.sequence(futures)){
        formatResults(output, results)
      }
    }

    main(container, handle2)
  }
}
