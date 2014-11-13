
import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.VDomBuilder

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
    var i = 0
    def recurse(t: Tree[String]): Tree[(String, String, Int)] = {
      val curr = (t.name, munge(t.name), i)
      i += 1
      val children = t.children.map(recurse)
      Tree(curr, children)
    }


    val Seq(main, menu, layout, menuLink) = Seq(
      "main", "menu", "layout", "menuLink"
    ).map(dom.document.getElementById)

    val snippets = dom.document.getElementsByClassName("highlight-me")

    snippets.foreach(js.Dynamic.global.hljs.highlightBlock(_))

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


    val menuBar = React.renderComponent(
      Menu(recurse(structure)),
      menu
    )
    menuLink.onclick = (e: dom.MouseEvent) => {
      toggleClass(layout, "active")
      toggleClass(menu, "active")
      toggleClass(menuLink, "active")
    }

    var scrolling = false
    main.onscroll = (e: dom.UIEvent) => {
      if (!scrolling){
        scrolling = true
        dom.requestAnimationFrame{(d: Double) =>
          scrolling = false
          val threshold = main.scrollTop + main.clientHeight

          var index = 0
          while(index < headers.length && index >= 0){
            index += 1
            if (headers(index) > threshold) index *= -1
          }
          index = -index
          menuBar.setState(index)
        }
      }
    }
  }
  def isElementInViewport(el: dom.HTMLElement) = {
    val rect = el.getBoundingClientRect()
    rect.top >= 0 && rect.bottom <= dom.innerHeight
  }

  import japgolly.scalajs.react._ // React
  import vdom.ReactVDom._         // Scalatags → React virtual DOM
  import vdom.ReactVDom.all._     // Scalatags html & css (div, h1, textarea, etc.)

  val Menu = ReactComponentB[Tree[(String, String, Int)]]("Menu")
                            .getInitialState(_ => 0)
                            .render{ (structure, _, index) =>

    val contentList = {
      var i = 0
      def rec1(current: Tree[(String, String, Int)]): Tree[(String, String, Int, Boolean)] = {
        val initialI = i
        i += 1
        val children = current.children.map(rec1)

        val win = i >= index && initialI < index
        Tree(
          (current.name._1, current.name._2, current.name._3, win),
          children
        )
      }
      def rec(current: Tree[(String, String, Int, Boolean)],
              depth: Int,
              classes: String): Iterator[Tag] = {

        val winIndex = current.children.indexWhere(_.name._4)
        val (before, after) = current.children.splitAt(winIndex)

        val myCls =
          "menu-item" +
          (if (depth <= 1) " menu-item-divided" else "")

        val (name, munged, currIndex, win) = current.name
        val frag =
          li(paddingLeft := s"${depth * 15}px")(
            a(
              name,
              href:="#"+munged,
              cls:=myCls
            ),
            cls:=classes + (if (win) " pure-menu-selected" else "")
          )
        val afterCls = if (!win) "hide" else ""
        Iterator(frag) ++
        before.flatMap(rec(_, depth+1, "lined")) ++
        after.flatMap(rec(_, depth+1, afterCls))
      }
      rec(rec1(structure), 0, "")
    }

    val frag = div(cls:="pure-menu  pure-menu-open")(
      a(cls:="pure-menu-heading", href:="#")(
        "Contents"
      ),
      ul(cls:="menu-item-list")(
        contentList.drop(1).toVector
      )
    )
    frag
  }.build
}