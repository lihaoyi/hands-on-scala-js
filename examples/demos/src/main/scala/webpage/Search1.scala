package webpage

import org.scalajs.dom
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import dom.html
@JSExport
object Search1 extends{
  @JSExport
  def main(target: html.Div) = {
    val listings = Seq(
      "Apple", "Apricot", "Banana", "Cherry",
      "Mango", "Mangosteen", "Mandarin",
      "Grape", "Grapefruit", "Guava"
    )

    def renderListings = ul(
      for {
        fruit <- listings
        if fruit.toLowerCase.startsWith(
          box.value.toLowerCase
        )
      } yield {
        val (first, last) = fruit.splitAt(
          box.value.length
        )
        li(
          span(
            backgroundColor:="yellow",
            first
          ),
          last
        )
      }
    ).render


    lazy val box = input(
      `type`:="text",
      placeholder:="Type here!"
    ).render

    val output = div(renderListings).render

    box.onkeyup = (e: dom.Event) => {
      output.innerHTML = ""
      output.appendChild(renderListings)
    }

    target.appendChild(
      div(
        h1("Search Box!"),
        p(
          "Type here to filter " +
          "the list of things below!"
        ),
        div(box),
        output
      ).render
    )
  }
}