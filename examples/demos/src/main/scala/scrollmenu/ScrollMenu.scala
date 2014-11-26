package scrollmenu

import org.scalajs.dom

import scala.scalajs.js
import scalatags.JsDom.all._

case class Tree[T](value: T, children: Vector[Tree[T]])

case class MenuNode(frag: dom.HTMLElement, id: String, start: Int, end: Int)

/**
 * High performance scrollspy to work keep the left menu bar in sync.
 * Lots of sketchy imperative code in order to maximize performance.
 */
class ScrollSpy(structure: Tree[String],
                main: dom.HTMLElement,
                var clean: Boolean = false){
  val (headers, domTrees) = {
    var i = -1
    def recurse(t: Tree[String], depth: Int): Tree[MenuNode] = {
      val curr =
        li(
          a(
            t.value,
            href:="#"+Controller.munge(t.value),
            cls:="menu-item"
          )
        )
      val originalI = i
      i += 1
      val children = t.children.map(recurse(_, depth + 1))
      Tree(
        MenuNode(
          curr(ul(marginLeft := "15px",children.map(_.value.frag))).render,
          Controller.munge(t.value),
          originalI,
          if (children.length > 0) children.map(_.value.end).max else originalI + 1
        ),
        children
      )
    }
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
      menuItems.map(Controller.munge)
        .map(dom.document.getElementById)
        .map(offset(_, main))
        .toVector
    }
    val domTrees = recurse(structure, 0)
    (headers, domTrees)
  }


  private[this] var scrolling = false
  def apply() = {
    if (!scrolling) {
      scrolling = true
      dom.requestAnimationFrame((d: Double) => start())
    }
  }
  private[this] var previousWin: MenuNode = null
  private[this] def start() = {
    scrolling = false
    def scroll(el: dom.Element) = {
      val rect = el.getBoundingClientRect()
      if (rect.top <= 0)
        el.scrollIntoView(true)
      else if (rect.top > dom.innerHeight)
        el.scrollIntoView(false)
    }
    val scrollTop = main.scrollTop
    def walkIndex(tree: Tree[MenuNode]): List[Tree[MenuNode]] = {
      val t @ Tree(m, children) = tree
      val win = if(m.start == -1) true
      else {
        val before = headers(m.start) <= scrollTop
        val after = (m.end >= headers.length) || headers(m.end) > scrollTop
        before && after
      }
      val childIndexes = children.map(walkIndex)
      val childWin = childIndexes.indexWhere(_ != null)
      if (childWin != -1) t :: childIndexes(childWin)
      else if (win) Nil
      else null
    }

    val winPath = walkIndex(domTrees)
    val winItem = winPath.last.value
    def walkTree(tree: Tree[MenuNode], indices: List[Tree[MenuNode]]): Unit = {
      for(item <- indices){
        item.value.frag.classList.remove("hide")
        item.value.frag.classList.remove("selected")
        item.value.frag.children(0).classList.add("pure-menu-selected")
        for(child <- item.children){
          val childFrag = child.value.frag
          childFrag.children(0).classList.remove("pure-menu-selected")
          childFrag.classList.add("hide")
          if (child.value.start < winItem.start) childFrag.classList.add("selected")
          else childFrag.classList.remove("selected")
        }
      }

    }

    if (winItem != previousWin){
      scroll(winItem.frag.children(0))
      dom.history.replaceState(null, null, "#" + winItem.id)
      previousWin = winItem
      walkTree(domTrees, winPath)
    }

  }


}