package scrollmenu

import org.scalajs.dom
import org.scalajs.dom.extensions._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._



@JSExport
object Controller{

  def munge(name: String) = {
    name.replace(" ", "")
  }

  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Tree[String]](upickle.json.readJs(data))

    val Seq(main, menu, layout, menuLink) = Seq(
      "main", "menu", "layout", "menuLink"
    ).map(dom.document.getElementById)

    val snippets = dom.document.getElementsByClassName("highlight-me")

    snippets.foreach(js.Dynamic.global.hljs.highlightBlock(_))

    val scrollSpy = new ScrollSpy(structure, main)

    menu.appendChild(
      div(cls:="pure-menu  pure-menu-open")(
        a(cls:="pure-menu-heading", href:="#")(
          "Contents"
        ),
        ul(cls:="menu-item-list")(
          scrollSpy.domTrees.map(_.value.frag)
        )
      ).render
    )

    menuLink.onclick = (e: dom.MouseEvent) => {
      layout.classList.toggle("active")
      menu.classList.toggle("active")
      menuLink.classList.toggle("active")
    }

    main.onscroll = (e: dom.UIEvent) => {
      scrollSpy(main.scrollTop + main.clientHeight)
    }
  }

}
