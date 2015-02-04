package advanced

import org.scalajs.dom
import scalajs.js
import scalajs.js.annotation.JSExport
import rx._
import scalatags.JsDom.all._
import dom.html
@JSExport
object BasicRx {
  @JSExport
  def main(container: html.Div) = {
    val txt = Var("")
    val numChars = Rx{txt().length}
    val numWords = Rx{
      txt().split(' ')
           .filter(_.length > 0)
           .length
    }

    val avgWordLength = Rx{
      txt().count(_ != ' ') * 1.0 / numWords()
    }

    val txtInput = textarea.render
    txtInput.onkeyup = (e: dom.Event) => {
      txt() = txtInput.value
    }

    container.appendChild(
      div(
        txtInput,
        ul(
          li("Chars: ", numChars),
          li("Words: ", numWords),
          li("Word Length: ", avgWordLength)
        )
      ).render
    )
  }
  @JSExport
  def main2(container: html.Div) = {
    val fruits = Seq(
      "Apple", "Apricot", "Banana", "Cherry",
      "Mango", "Mangosteen", "Mandarin",
      "Grape", "Grapefruit", "Guava"
    )
    val query = Var("")
    val txtInput = input.render
    txtInput.onkeyup = (e: dom.Event) => {
      query() = txtInput.value
    }

    val fragments =
      for(fruit <- fruits) yield Rx {
        val shown = fruit.toLowerCase
                         .startsWith(query())
        if (shown) li(fruit)
        else li(display := "none")
      }

    container.appendChild(
      div(
        txtInput,
        ul(fragments)
      ).render
    )
  }
  implicit def rxFrag[T <% Frag](r: Rx[T]): Frag = {
    def rSafe: dom.Node = span(r()).render
    var last = rSafe
    Obs(r, skipInitial = true){
      val newLast = rSafe
      js.Dynamic.global.last = last
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    last
  }
}
