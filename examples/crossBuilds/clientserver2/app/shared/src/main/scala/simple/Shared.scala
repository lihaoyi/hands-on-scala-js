package simple

import upickle.default._

case class FileData(name: String, size: Long)

object FileData {
  implicit val rw: ReadWriter[FileData] = macroRW
}

trait Api{
  def list(path: String): Seq[FileData]
}
