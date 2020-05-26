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
import scalajs.concurrent.JSExecutionContext.Implicits.runNow

@JSExportTopLevel("AdvancedFutures")
object Futures {
  def main(container: html.Div,
            handle: (Seq[String], html.Div) => Unit) = {
    val myInput = input(value:="London,Singapore,Berlin,New York").render
    val output = div.render
    myInput.onkeyup = (e: dom.KeyboardEvent) => {
      if (e.keyCode == KeyCode.enter){
        handle(myInput.value.split(','), output)
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
  def urlFor(name: String) = {
    "http://api.openweathermap.org/data/" +
    "2.5/find?mode=json&q=" +
    name
  }
  def parseTemp(text: String) = {
    val data = js.JSON.parse(text)
    val kelvins = data.list
                      .pop()
                      .main
                      .temp
                      .asInstanceOf[Double]
    kelvins - 272.15
  }
  def formatResults(output: html.Element, results: Seq[(String, Double)]) = {
    output.innerHTML = ""
    output.appendChild(ul(
      for((name, temp) <- results) yield li(
        b(name), " - ", temp.toInt, "C"
      )
    ).render)
  }
  @JSExport
  def main0(container: html.Div) = {
    def handle0(names: Seq[String], output: html.Div) = {
      val results = mutable.Buffer.empty[(String, Double)]
      for(name <- names){
        val xhr = new XMLHttpRequest
        xhr.open("GET", urlFor(name))
        xhr.onload = (e: dom.Event) => {
          val temp = parseTemp(xhr.responseText)
          results.append((name, temp))
          if (results.length == names.length){
            formatResults(output, results)
          }
        }
        xhr.send()
      }
    }
    main(container, handle0)
  }
  @JSExport
  def main1(container: html.Div) = {
    def handle1(names: Seq[String], output: html.Div) = {
      val results = mutable.Buffer.empty[(String, Double)]
      for{
        name <- names
        xhr <- Ajax.get(urlFor(name))
      } {
        val temp = parseTemp(xhr.responseText)
        results.append((name, temp))
        if (results.length == names.length){
          formatResults(output, results)
        }
      }
    }
    main(container, handle1)
  }
  @JSExport
  def main2(container: html.Div) = {
    def handle2(names: Seq[String], output: html.Div) = {
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
