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
  var indent = 0


  val headers = Seq[(String => scalatags.Text.Tag, Option[Frag => Frag])](
    (h => div(cls:="header")(
      h1(h),
      h2("Writing client-side web applications in Scala")
    ), Some(f => div(cls:="content", f))),
    (h => div(cls:="header")(
      h1(id:=Utils.munge(h), h),
      br
    ), None),
    (h1(_), None),
    (h2(_), None),
    (h3(_), None),
    (h4(_), None),
    (h5(_), None),
    (h6(_), None)
  )

  var structure: Node = null
  case class sect(name: String){
    indent += 1
    val newNode = Node(name, mutable.Buffer.empty)
    val (headerWrap, contentWrap) = headers(indent-1)
    if (structure!= null) structure.children.append(newNode)
    val prev = structure
    structure = newNode
    def apply(args: Frag*) = {
      val wrappedContents = contentWrap.getOrElse((x: Frag) => x)(args)
      val res = Seq[Frag](
        headerWrap(name)(cls:="content-subhead", id:=munge(name)),
        wrappedContents
      )
      indent -= 1
      if (prev != null) structure = prev
      res
    }
  }
  def munge(name: String) = {
    name.replace(" ", "")
  }
}