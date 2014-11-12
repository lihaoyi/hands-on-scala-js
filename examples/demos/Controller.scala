
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import scalatags.JsDom.all._
import rx._
import util.{Success, Failure}
case class Node(name: String, children: mutable.Buffer[Node])
@JSExport
object Controller{

  def munge(name: String) = {
    name.replace(" ", "")
  }
  def toggleClass(el: dom.HTMLElement, cls: String) = {
    val frags = el.className.split(' ')
    if (!frags.contains(cls)) el.className = el.className + " " + cls
    else el.className = el.className.split(' ').filter(_ != cls).mkString(" ")
  }
  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Node](upickle.json.readJs(data))

    val main = dom.document.getElementById("main")
    val menu = dom.document.getElementById("menu")

    val layout   = dom.document.getElementById("layout")
    val menuLink = dom.document.getElementById("menuLink")
    val snippets = dom.document.getElementsByClassName("highlight-me")

    snippets.foreach(js.Dynamic.global.hljs.highlightBlock(_))

    val contentBar = {
      def rec(current: Node, depth: Int): Seq[dom.HTMLLIElement] = {
        println("\t"*depth + current.name)
        val myCls =
          "menu-item" +
          (if (depth == 1) " menu-item-divided" else "")

        val frag =
          li(
            a(
              current.name,
              href:="#"+munge(current.name),
              paddingLeft := s"${depth * 10 + 10}px",
              cls:=myCls
            )
          ).render

        frag +: current.children.flatMap(rec(_, depth + 1))
      }
      structure.children.flatMap(rec(_, 0))
    }
    def menuItems = {
      def rec(current: Node): Seq[String] = {
        current.name +: current.children.flatMap(rec)
      }
      rec(structure).tail
    }
    val frag = div(cls:="pure-menu pure-menu-open")(
      a(cls:="pure-menu-heading", href:="#")(
        "Contents"
      ),
      ul(cls:="menu-item-list")(
        contentBar
      )
    )
    menu.appendChild(frag.render)

    def offset(el: dom.HTMLElement, parent: dom.HTMLElement): Double = {
      if (el == parent) 0
      else el.offsetTop + offset(el.offsetParent.asInstanceOf[dom.HTMLElement], parent)
    }
    val headers = menuItems.map(munge)
                           .map(dom.document.getElementById)
                           .map(offset(_, main))
                           .toArray

    scrollSpy(main, headers, contentBar)

    menuLink.onclick = (e: dom.MouseEvent) => {
      toggleClass(layout, "active")
      toggleClass(menu, "active")
      toggleClass(menuLink, "active")
    };
  }



  def scrollSpy(main: dom.HTMLElement,
                headers: Seq[Double],
                contentBar: Seq[dom.HTMLElement]) = {
    def isElementInViewport(el: dom.HTMLElement) = {
      val rect = el.getBoundingClientRect()
      rect.top >= 0 && rect.bottom <= dom.innerHeight
    }

    var scrolling = false
    var lastIndex = -1
    def run() = {
      scrolling = false
      val threshold = main.scrollTop + main.clientHeight
      var index = 0
      while(index < headers.length && index >= 0){
        index += 1
        if (headers(index) > threshold) index *= -1
      }
      index = -index - 1
      if (index != lastIndex){
        if (!isElementInViewport(contentBar(index))) {
          contentBar(index).scrollIntoView(lastIndex > index)
        }
        if (lastIndex != -1)
          toggleClass(contentBar(lastIndex), "pure-menu-selected")
        toggleClass(contentBar(index), "pure-menu-selected")
        lastIndex = index
      }
    }
    run()
    main.onscroll = (e: dom.UIEvent) => {
      if (!scrolling){
        scrolling = true
        dom.requestAnimationFrame((d: Double) => run())
      }
    }
  }
}
