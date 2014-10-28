package book


import scala.collection.mutable
import scalatags.Text.all._

case class Node(name: String, children: mutable.Buffer[Node])
object Utils{
  val autoResources = Seq(
    "META-INF/resources/webjars/highlightjs/8.2-1/highlight.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/styles/idea.min.css",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/scala.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/javascript.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/bash.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/diff.min.js",
    "META-INF/resources/webjars/highlightjs/8.2-1/languages/xml.min.js",
    "css/pure-min.css",
    "css/grids-responsive-min.css",
    "css/layouts/side-menu.css",
    "js/ui.js"
  )
  
  val manualResources = Seq(
    "images/javascript-the-good-parts-the-definitive-guide.jpg",
    "images/Hello World.png",
    "images/Hello World White.png",
    "images/Hello World Console.png",
    "images/IntelliJ Hello.png",
    "example-fastopt.js"
  )
  
  val includes = for(res <- Utils.autoResources) yield {
    if (res.endsWith(".js"))
      script(src:=res)
    else if (res.endsWith(".css"))
      link(rel:="stylesheet", href:=res)
    else
      raw("")
  }
  println(includes)
  var indent = 1
  val headers = Seq(h1, h1, h2, h3, h4, h5, h6)
  val structure = Node("Hands-on Scala.js", mutable.Buffer.empty)
  var current = structure
  case class sect(name: String){
    indent += 1
    val newNode = Node(name, mutable.Buffer.empty)
    current.children.append(newNode)
    val prev = current
    current = newNode
    def apply(args: Frag*) = {
      val res = Seq(
        headers(indent-1)(cls:="content-subhead", id:=munge(name), name) +: args:_*
      )
      indent -= 1
      current = prev
      res
    }
  }
  def munge(name: String) = {
    name.replace(" ", "")
  }

}