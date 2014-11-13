
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import scalatags.JsDom.all._

object Renderer{
  import japgolly.scalajs.react._ // React
  import vdom.ReactVDom._         // Scalatags → React virtual DOM
  import vdom.ReactVDom.all._     // Scalatags html & css (div, h1, textarea, etc.)

  val Menu = ReactComponentB[Int]("Menu").render{ p =>

  }.build
}
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
            paddingLeft := s"${depth * 15}px",
            a(
              current.name,
              href:="#"+munge(current.name),
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
        .toVector
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
                headers: Vector[Double],
                contentList: Vector[dom.HTMLElement],
                contentTree: Tree[dom.HTMLElement]) = {



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
      index = -index

      if (index != lastIndex){
        updateSideBar(lastIndex, index, contentList, contentTree)
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
  def isElementInViewport(el: dom.HTMLElement) = {
    val rect = el.getBoundingClientRect()
    rect.top >= 0 && rect.bottom <= dom.innerHeight
  }
  val lastShown = new js.Array[dom.HTMLElement](0)
  val lastLined = new js.Array[dom.HTMLElement](0)
  def updateSideBar(lastIndex: Int,
                    index: Int,
                    contentList: Vector[dom.HTMLElement],
                    contentTree: Tree[dom.HTMLElement]) = {

    println(s"MOVING $lastIndex -> $index")
    if (!isElementInViewport(contentList(index))) {
      contentList(index).scrollIntoView(lastIndex > index)
    }
    val shown = new js.Array[dom.HTMLElement](0)
    val lined = new js.Array[dom.HTMLElement](0)
    /**
     * Makes two passes over the children list; once to determine if
     * the current element is a parent of the current header, and another
     * to mark all the children of the current element with the correct
     * CSS classes.
     */
    def rec(curr: Tree[dom.HTMLElement]): Boolean = {

      var found = false
      var i = 0
      var j = 0
      while (j < curr.children.length){
        val x = curr.children(j)
        j+= 1
        val f = rec(x)
        found |= f
        if (!found) i += 1
      }

      if (found || curr.name == contentList(index)){
        var j = 0
        while (j < curr.children.length){
          val x = curr.children(j)
          if (found && i > 0){
            lined.push(x.name)
            i -= 1
          }

          j+= 1
          shown.push(x.name)
        }
        lined.push(curr.name)
        true
      }else false

    }
    rec(contentTree)
    for(el <- contentList){
      if (shown.indexOf(el) != -1) removeClass(el, "hide")
      else addClass(el, "hide")

      if (lined.indexOf(el) == -1) removeClass(el, "lined")
      else addClass(el, "lined")
    }
    if (lastIndex != -1) removeClass(contentList(lastIndex), "pure-menu-selected")
    addClass(contentList(index), "pure-menu-selected")
  }
}
