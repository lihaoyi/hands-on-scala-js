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
    val list = ul(cls:="menu-item-list collapsed")(
      scrollSpy.domTrees.map(_.value.frag)
    ).render

    def updateScroll() = scrollSpy(main.scrollTop + main.clientHeight)
    val expandIcon = i(cls:="fa fa-caret-down").render
    val expandLink =
      a(
        expandIcon,
        href:="javascript:",
        marginLeft:="0px",
        paddingLeft:="15px",
        paddingRight:="15px",
        cls:="pure-menu-selected",
        onclick := { (e: dom.Event) =>
          expandIcon.classList.toggle("fa-caret-down")
          expandIcon.classList.toggle("fa-caret-up")
          list.classList.toggle("collapsed")
          scrollSpy.clean = !scrollSpy.clean
          updateScroll()
        }
      ).render


    menu.appendChild(
      div(
        zIndex:=10,
        position:="absolute",
        cls:="pure-menu pure-menu-open",
        ul(cls:="menu-item-list")(
          li(
            width:="43px",
            float:="right",
            expandLink
          )
        )
      ).render
    )

    menu.appendChild(
      div(cls:="pure-menu  pure-menu-open")(
        a(cls:="pure-menu-heading")(
          "Contents"
        ),
        list
      ).render
    )

    menuLink.onclick = (e: dom.MouseEvent) => {
      layout.classList.toggle("active")
      menu.classList.toggle("active")
      menuLink.classList.toggle("active")
    }

    main.onscroll = (e: dom.UIEvent) => updateScroll()
    updateScroll()
  }

}
