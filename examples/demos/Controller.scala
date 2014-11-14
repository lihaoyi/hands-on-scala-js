
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import scalatags.JsDom.all._

case class Tree[T](name: T, children: Vector[Tree[T]])

@JSExport
object Controller{

  def munge(name: String) = {
    name.replace(" ", "")
  }

  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Tree[String]](upickle.json.readJs(data))
    var i = 0
    def recurse(t: Tree[String], depth: Int): Tree[(dom.HTMLElement, Int, Int)] = {
      val curr =
        li(
          a(
            t.name,
            href:="#"+munge(t.name),
            cls:="menu-item"
          )
        )
      val originalI = i
      i += 1
      val children = t.children.map(recurse(_, depth + 1))
      Tree(
        (
          curr(ul(paddingLeft := "15px",children.map(_.name._1))).render,
          originalI,
          if (children.length > 0) children.map(_.name._3).max else originalI + 1
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
          current.name +: current.children.flatMap(rec)
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

    menu.appendChild(
      div(cls:="pure-menu  pure-menu-open")(
        a(cls:="pure-menu-heading", href:="#")(
          "Contents"
        ),
        ul(cls:="menu-item-list")(
          domTrees.map(_.name._1)
        )
      ).render
    )
    menuLink.onclick = (e: dom.MouseEvent) => {
      layout.classList.toggle("active")
      menu.classList.toggle("active")
      menuLink.classList.toggle("active")
    }

    var scrolling = false
    var lastSelected: dom.HTMLElement = null
    def start() ={
      scrolling = false
      val threshold = main.scrollTop + main.clientHeight
      def walkTree(tree: Tree[(dom.HTMLElement, Int, Int)]): Boolean = {
        val Tree((menuItem, index, next), children) = tree
        val before = headers(index) < threshold
        val after = (next >= headers.length) || headers(next) > threshold
        val win = before && after
        if (win){
          menuItem.classList.remove("hide")
          var winFound = false

          for(c <- tree.children){
            val newWinFound = walkTree(c)
            if (!winFound) c.name._1.classList.add("selected")
            else c.name._1.classList.remove("selected")
            winFound = winFound | newWinFound
          }
          if (!winFound) {
            tree.children.foreach(_.name._1.classList.remove("selected"))
            if (lastSelected != null)
              lastSelected.children(0).classList.remove("pure-menu-selected")
            menuItem.children(0).classList.add("pure-menu-selected")
            lastSelected = menuItem
          }
        }else{
          menuItem.classList.add("hide")
          menuItem.classList.remove("selected")
        }
        win
      }
      domTrees.map(walkTree)
    }
    main.onscroll = (e: dom.UIEvent) => if (!scrolling){
      scrolling = true
      dom.requestAnimationFrame((d: Double) => start())
    }
  }
  def isElementInViewport(el: dom.HTMLElement) = {
    val rect = el.getBoundingClientRect()
    rect.top >= 0 && rect.bottom <= dom.innerHeight
  }


}