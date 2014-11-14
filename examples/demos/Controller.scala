import acyclic.file
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import scalatags.JsDom.all._



@JSExport
object Controller{

  def munge(name: String) = {
    name.replace(" ", "")
  }

  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Tree[String]](upickle.json.readJs(data))
    var i = 0
    def recurse(t: Tree[String], depth: Int): Tree[MenuNode] = {
      val curr =
        li(
          a(
            t.value,
            href:="#"+munge(t.value),
            cls:="menu-item"
          )
        )
      val originalI = i
      i += 1
      val children = t.children.map(recurse(_, depth + 1))
      Tree(
        MenuNode(
          curr(ul(paddingLeft := "15px",children.map(_.value.frag))).render,
          originalI,
          if (children.length > 0) children.map(_.value.end).max else originalI + 1
        ),
        children
      )
    }

    val Seq(main, menu, layout, menuLink) = Seq(
      "main", "menu", "layout", "menuLink"
    ).map(dom.document.getElementById)

    val snippets = dom.document.getElementsByClassName("highlight-me")

    snippets.foreach(js.Dynamic.global.hljs.highlightBlock(_))
    def offset(el: dom.HTMLElement, parent: dom.HTMLElement): Double = {
      if (el == parent) 0
      else el.offsetTop + offset(el.offsetParent.asInstanceOf[dom.HTMLElement], parent)
    }
    val headers = {

      val menuItems = {
        def rec(current: Tree[String]): Seq[String] = {
          current.value +: current.children.flatMap(rec)
        }
        rec(structure).tail
      }
      menuItems.map(munge)
        .map(dom.document.getElementById)
        .map(offset(_, main))
        .toVector
    }
    println(headers)



    val domTrees = structure.children.map(recurse(_, 0))
    val scrollSpy = new ScrollSpy(headers, domTrees)
    menu.appendChild(
      div(cls:="pure-menu  pure-menu-open")(
        a(cls:="pure-menu-heading", href:="#")(
          "Contents"
        ),
        ul(cls:="menu-item-list")(
          domTrees.map(_.value.frag)
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
  def isElementInViewport(el: dom.HTMLElement) = {
    val rect = el.getBoundingClientRect()
    rect.top >= 0 && rect.bottom <= dom.innerHeight
  }
}
