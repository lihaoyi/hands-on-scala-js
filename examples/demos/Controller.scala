
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
  def addClass(el: dom.HTMLElement, cls: String) = {
    removeClass(el, cls)
    el.className = el.className + " " + cls
  }
  def removeClass(el: dom.HTMLElement, cls: String) = {
    el.className = el.className.split(' ')
                               .iterator
                               .filter(_ != cls)
                               .mkString(" ")
  }
  def toggleClass(el: dom.HTMLElement, cls: String) = {
    val frags = el.className.split(' ')
    if (!frags.contains(cls)) addClass(el, cls)
    else removeClass(el, cls)
  }
  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Tree[String]](upickle.json.readJs(data))
    var i = 0
    def recurse(t: Tree[String], depth: Int): Tree[(dom.HTMLElement, Int)] = {
      val curr =
        li(paddingLeft := "15px")(
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
          curr(ul(children.map(_.name._1))).render,
          originalI
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

    val domTrees = recurse(structure, 0).children

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
      toggleClass(layout, "active")
      toggleClass(menu, "active")
      toggleClass(menuLink, "active")
    }

    var scrolling = false

    def start() ={
      scrolling = false
      val threshold = main.scrollTop + main.clientHeight
      println("")
      def walkTree(tree: Tree[(dom.HTMLElement, Int)]): Unit = {
        val Tree((menuItem, index), children) = tree
        val before = headers(index) < threshold

        val next = children.lastOption
                           .fold(index)(_.name._2)

        val win = before && headers.lift(next + 1).getOrElse(999999.0) > threshold

        if (win){
          removeClass(menuItem, "hide")
          addClass(menuItem, "selected")
          tree.children.foreach(walkTree)
        }else{
          addClass(menuItem, "hide")
          removeClass(menuItem, "selected")
        }
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