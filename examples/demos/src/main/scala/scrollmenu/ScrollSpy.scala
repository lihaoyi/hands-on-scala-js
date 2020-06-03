package scrollmenu

import org.scalajs.dom
import dom.html
import org.scalajs.dom.ext._
import scalajs.js
import scalatags.JsDom.all._
import upickle.default._

case class Tree[T](value: T, children: Vector[Tree[T]])

object Tree {
  implicit def rw[T: ReadWriter]: ReadWriter[Tree[T]] = macroRW
}

case class MenuNode(frag: html.Element, id: String, start: Int, end: Int)

/**
 * High performance scrollspy to work keep the left menu bar in sync.
 * Lots of sketchy imperative code in order to maximize performance.
 */
class ScrollSpy(structure: Tree[String],
                main: html.Element){
  lazy val domTrees = {
    var i = -1
    def recurse(t: Tree[String], depth: Int): Tree[MenuNode] = {
      val curr =
        li(
          a(
            t.value,
            display := (if (i == -1) "none" else "block"),
            href:="#"+Controller.munge(t.value),
            cls:="menu-item"
          )
        )
      val originalI = i
      i += 1
      val children = t.children.map(recurse(_, depth + 1))
      Tree(
        MenuNode(
          curr(ul(paddingLeft := "15px",children.map(_.value.frag))).render,
          Controller.munge(t.value),
          originalI,
          if (children.length > 0) children.map(_.value.end).max else originalI + 1
        ),
        children
      )
    }

    val domTrees = recurse(structure, 0)
    domTrees
  }
  def offset(el: html.Element, parent: html.Element): Double = {
    if (el == parent) 0
    else el.offsetTop + offset(el.offsetParent.asInstanceOf[html.Element], parent)
  }
  lazy val headers = {
    val menuItems = {
      def rec(current: Tree[String]): Seq[String] = {
        current.value +: current.children.flatMap(rec)
      }
      rec(structure).tail
    }

    val offsets = for(name <- menuItems) yield {
      val el = dom.document.getElementById(Controller.munge(name)).asInstanceOf[html.Element]
      () => offset(el, dom.document.body)

    }
    js.Array(offsets:_*)
  }

  var open = false
  def toggleOpen() = {
    open = !open
    if (open){
      def rec(tree: Tree[MenuNode])(f: MenuNode => Unit): Unit = {
        f(tree.value)
        tree.children.foreach(rec(_)(f))
      }
      rec(domTrees)(setFullHeight)
    }else{
      start(force = true)
    }
  }

  def setFullHeight(mn: MenuNode) = {
    mn.frag
      .children(1)
      .asInstanceOf[html.Element]
      .style
      .maxHeight = s"${(mn.end - mn.start + 1) * 44}px"
  }
  private[this] var scrolling = false
  private[this] var scrollTop = -1
  def apply(): Unit = {
    start()
  }
  private[this] var previousWin: MenuNode = null
  private[this] def start(force: Boolean = false) = {
    val scrollTop = dom.document.body.scrollTop
    def walkIndex(tree: Tree[MenuNode]): List[Tree[MenuNode]] = {
      val t @ Tree(m, children) = tree
      val win = if(m.start == -1) true
      else {
        val before = headers(m.start)() <= scrollTop
        val after = (m.end >= headers.length) || headers(m.end)() > scrollTop
        before && after
      }
      val childIndexes = children.map(walkIndex)
      val childWin = childIndexes.indexWhere(_ != null)
      if (childWin != -1) t :: childIndexes(childWin)
      else if (win) List(t)
      else null
    }

    val winPath = walkIndex(domTrees)

    val winItem = winPath.last.value
    def walkTree(indices: List[Tree[MenuNode]]): Int = indices match {
      case Nil => 0
      case (Tree(mn, children) :: rest) =>

        mn.frag.classList.remove("hide")
        mn.frag.classList.remove("selected")

        mn.frag.children(0).classList.add("pure-menu-selected")
        for {
          child <- children
          if !indices.headOption.exists(_.value.frag == child.value.frag)
        } {
          walkHide(child)
        }
        val size = walkTree(rest) + children.length
        mn.frag.children(1).asInstanceOf[html.Element].style.maxHeight =
          if (!open) s"${size * 44}px" else "none"
        size
    }

    def walkHide(tree: Tree[MenuNode]): Unit = {
      val frag = tree.value.frag

      frag.children(0).classList.remove("pure-menu-selected")
      frag.classList.add("hide")

      frag.children(1).asInstanceOf[html.Element].style.maxHeight =
        if (!open) "0px" else "none"

      if (tree.value.start < winItem.start) frag.classList.add("selected")
      else frag.classList.remove("selected")
      tree.children.foreach(walkHide)
    }

    if (winItem != previousWin || force){
      previousWin = winItem
      walkTree(winPath)
    }
  }
}
