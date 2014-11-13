
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.extensions._
import scala.collection.mutable
import japgolly.scalajs.react._ // React

import vdom.ReactVDom.all._     // Scalatags html & css (div, h1, textarea, etc.)


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
    def recurse(t: Tree[String], depth: Int): Tree[(Tag, Int)] = {
      val curr =
        li(paddingLeft := s"${depth * 15}px")(
          a(
            t.name,
            href:=munge("#"+t.name),
            cls:="menu-item"
          )
        )
      val originalI = i
      i += 1
      val children = t.children.map(recurse(_, depth + 1))
      Tree(curr -> originalI, children)
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
      Menu(recurse(structure, 0)),
      menu
    )
    menuLink.onclick = (e: dom.MouseEvent) => {
      toggleClass(layout, "active")
      toggleClass(menu, "active")
      toggleClass(menuLink, "active")
    }

    var x = -1
    def start() =
      x = dom.setTimeout(() => {
        x = -1
        val threshold = main.scrollTop + main.clientHeight

        var index = 0
        while(index < headers.length && index >= 0){
          index += 1
          if (headers(index) > threshold) index *= -1
        }
        index = -index
        menuBar.setState(index)

      }, 100)

    main.onscroll = (e: dom.UIEvent) => {
      if (x != -1){
        dom.clearTimeout(x)
        s
      }
      start()
    }
  }
  def isElementInViewport(el: dom.HTMLElement) = {
    val rect = el.getBoundingClientRect()
    rect.top >= 0 && rect.bottom <= dom.innerHeight
  }



  val Menu = ReactComponentB[Tree[(Tag, Int)]]("Menu")
                            .getInitialState(_ => 0)
                            .render{ (structure, _, index) =>

    val contentList = {
      var i = 0
      val winArray = new js.Array[Boolean](0)
      def rec1(current: Tree[(Tag, Int)]): Unit = {
        val initialI = i
        i += 1
        current.children.foreach(rec1)
        winArray(current.name._2) = i >= index && initialI < index
      }

      val output = new js.Array[Tag](0)
      def rec(current: Tree[(Tag, Int)],
              classes: String): Unit = {


        val winIndex = current.children.indexWhere { x =>
          winArray(x.name._2)
        }
        val (before, after) = current.children.splitAt(winIndex)

        val (tag, currIndex) = current.name

        val win = winArray(currIndex)

        val frag = tag(
          cls:=classes + (if (win) " pure-menu-selected" else "")
        )

        output.push(frag)

        before.foreach(rec(_, "lined"))
        after.foreach(rec(_, if (!win) "hide" else ""))
      }
      rec1(structure)
      rec(structure, "")
      output
    }

    val frag = div(cls:="pure-menu  pure-menu-open")(
      a(cls:="pure-menu-heading", href:="#")(
        "Contents"
      ),
      ul(cls:="menu-item-list")(
        contentList.drop(1):_*
      )
    )
    frag
  }.build
}