package book

import acyclic.file
import scala.collection.mutable
import scalatags.Text.all._
case class pureTable(header: Frag*){
  def apply(content: Frag*) = {
    table(cls:="pure-table pure-table-horizontal half-table")(
      thead(header),
      tbody(content)
    )
  }
}
object sect{

  var indent = 0

  val headers = Seq[((String, String, Frag) => scalatags.Text.Tag, Option[Frag => Frag])](
    ((h, s, l) => div(cls:="header")(
      h1(h, l),
      h2(s)
    ), Some(f => div(cls:="content", f))),
    ((h, s, l) => div(cls:="header")(
      h1(id:=munge(h), h, l),
      br
    ), None),
    (h1(_, _, _), None),
    (h2(_, _, _), None),
    (h3(_, _, _), None),
    (h4(_, _, _), None),
    (h5(_, _, _), None),
    (h6(_, _, _), None)
  )

  var structure = Node("root", mutable.Buffer.empty)

  val usedRefs = mutable.Set.empty[String]

  def ref(s: String) = {
    usedRefs += s
    a(s, href:=s"#${munge(s)}")
  }

  def munge(name: String) = {
    name.replace(" ", "")
  }
}
case class sect(name: String, subname: String = ""){
  sect.indent += 1
  val newNode = Node(name, mutable.Buffer.empty)
  val (headerWrap, contentWrap) = sect.headers(sect.indent-1)
  sect.structure.children.append(newNode)
  val prev = sect.structure
  sect.structure = newNode
  def apply(args: Frag*) = {
    val wrappedContents = contentWrap.getOrElse((x: Frag) => x)(args)

    val headingAnchor = a(
      cls:="header-link",
      href:=s"#${sect.munge(name)}",
      " ",
      i(cls:="fa fa-link")
    )
    val res = Seq[Frag](
      headerWrap(name, subname, headingAnchor)(
        cls:="content-subhead",
        id:=sect.munge(name)
      ),
      wrappedContents
    )
    sect.indent -= 1
    sect.structure = prev
    res
  }
}
case class Node(name: String, children: mutable.Buffer[Node])
object lnk{
  val usedLinks = mutable.Set.empty[String]
  def apply(name: String, url: String) = {
    usedLinks.add(url)
    a(name, href:=url)
  }
}

object hl{
  def highlight(snippet: Seq[String], lang: String) = {
    val string = snippet.mkString
    val lines = string.split("\n", -1)
    if (lines.length == 1){
      code(cls:=lang + " highlight-me", lines(0), padding:=0, display:="inline")
    }else{
      val minIndent = lines.map(_.takeWhile(_ == ' ').length)
        .filter(_ > 0)
        .min
      val stripped = lines.map(_.drop(minIndent))
        .dropWhile(_ == "")
        .mkString("\n")

      pre(code(cls:=lang + " highlight-me", stripped))
    }
  }

  def javascript(code: String*) = highlight(code, "javascript")
  def scala(code: String*) = highlight(code, "scala")
  def bash(code: String*) = highlight(code, "bash")
  def diff(code: String*) = highlight(code, "diff")
  def html(code: String*) = highlight(code, "xml")

  def ref(filepath: String, start: String = "", end: String = "\n") = {

    val lang = filepath.split('.').last match {
      case "js" => "javascript"
      case "scala" => "scala"
      case "sbt" => "scala"
      case "sh" => "bash"
      case "html" => "xml"
      case x =>
        println("??? " + x)
        ???
    }

    val lines = io.Source.fromFile(filepath).getLines().toVector

    def indent(line: String) = line.takeWhile(_.isWhitespace).length

    val startLine = lines.indexWhere(_.contains(start))
    if (startLine == -1){
      throw new Exception("Can't find marker: " + start)
    }
    val whitespace = indent(lines(startLine))
    val endLine = lines.indexWhere(
      line => line.contains(end) || (indent(line) < whitespace && line.trim != ""),
      startLine
    )
    val sliced =
      if (endLine == -1) lines.drop(startLine)
      else lines.slice(startLine, endLine)
    val blob = sliced.map(_.drop(whitespace)).mkString("\n")


    pre(code(cls:=lang + " highlight-me", blob))
  }
}