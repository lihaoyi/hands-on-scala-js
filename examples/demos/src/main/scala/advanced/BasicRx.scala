package advanced

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import rx._

@JSExportTopLevel("AdvancedBasicRx")
object BasicRx {
  implicit val ctxOwner: Ctx.Owner = Ctx.Owner.safe()

  @JSExport
  def main(container: html.Div): Unit = {
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

  implicit def rxFrag[T](r: Rx[T])(
    implicit conv: T => Frag, ctxOwner: Ctx.Owner): Frag = {

    val firstNode = r.now.render
    var lastNode: dom.Node = firstNode
    r.triggerLater { value =>
      val newNode = value.render
      lastNode.parentNode.replaceChild(newNode, lastNode)
      lastNode = newNode
    }
    firstNode
  }
}
