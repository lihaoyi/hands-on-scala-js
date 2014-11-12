
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import scalatags.JsDom.all._

case class Tree[T](name: T, children: Seq[Tree[T]])
@JSExport
object Controller{

  def munge(name: String) = {
    name.replace(" ", "")
  }
  def addClass(el: dom.HTMLElement, cls: String) = {
    println("Adding Class " + cls)
    removeClass(el, cls)
    el.className = el.className + " " + cls
  }
  def removeClass(el: dom.HTMLElement, cls: String) = {
    el.className = el.className.split(' ').filter(_ != cls).mkString(" ")
  }
  def toggleClass(el: dom.HTMLElement, cls: String) = {
    val frags = el.className.split(' ')
    if (!frags.contains(cls)) addClass(el, cls)
    else removeClass(el, cls)
  }
  @JSExport
  def main(data: scala.scalajs.js.Any) = {

    val structure = upickle.readJs[Tree[String]](upickle.json.readJs(data))

    val Seq(main, menu, layout, menuLink) = Seq(
      "main", "menu", "layout", "menuLink"
    ).map(dom.document.getElementById)

    val snippets = dom.document.getElementsByClassName("highlight-me")

    snippets.foreach(js.Dynamic.global.hljs.highlightBlock(_))

    val contentTree = {
      def rec(current: Tree[String], depth: Int): Tree[dom.HTMLElement] = {
        val myCls =
          "menu-item" +
          (if (depth <= 1) " menu-item-divided" else "")

        val frag =
          li(
            a(
              current.name,
              href:="#"+munge(current.name),
              paddingLeft := s"${depth * 15}px",
              cls:=myCls
            )
          ).render
        Tree(frag, current.children.map(rec(_, depth + 1)))
      }
      rec(structure, 0)
    }
    val contentList = {
      def rec(current: Tree[dom.HTMLElement]): Seq[dom.HTMLElement] = {
        current.name +: current.children.flatMap(rec)
      }
      rec(contentTree).toVector
    }

    val frag = div(cls:="pure-menu pure-menu-open")(
      a(cls:="pure-menu-heading", href:="#")(
        "Contents"
      ),
      ul(cls:="menu-item-list")(
        contentList.drop(1)
      )
    )
    menu.appendChild(frag.render)


    val headers = {
      def offset(el: dom.HTMLElement, parent: dom.HTMLElement): Double = {
        if (el == parent) 0
        else el.offsetTop + offset(el.offsetParent.asInstanceOf[dom.HTMLElement], parent)
      }
      val menuItems = {
        def rec(current: Tree[String]): Seq[String] = {
          current.name +: current.children.flatMap(rec)
        }
        rec(structure).tail
      }
      menuItems.map(munge)
        .map(dom.document.getElementById)
        .map(offset(_, main))
        .toArray
    }

    scrollSpy(main, headers, contentList, contentTree)

    menuLink.onclick = (e: dom.MouseEvent) => {
      toggleClass(layout, "active")
      toggleClass(menu, "active")
      toggleClass(menuLink, "active")
    }
  }

  /**
   * Needs to be done in a sketchy imperative fashion for performance:
   * onscroll gets called quite a lot, so any additional work makes it
   * noticeable jerky
   */
  def scrollSpy(main: dom.HTMLElement,
                headers: Seq[Double],
                contentList: Seq[dom.HTMLElement],
                contentTree: Tree[dom.HTMLElement]) = {

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
        if (!isElementInViewport(contentList(index))) {
          contentList(index).scrollIntoView(lastIndex > index)
        }
        def rec(curr: Tree[dom.HTMLElement]): Boolean = {
          if (curr.children.map(rec).contains(true) ||
              curr.name == contentList(index)){
            addClass(curr.name, "pure-menu-selected")
            curr.children.map(_.name).foreach(removeClass(_, "hide"))
            true
          }else{
            removeClass(curr.name, "pure-menu-selected")
            addClass(curr.name, "hide")
            false
          }
        }
        rec(contentTree)

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
