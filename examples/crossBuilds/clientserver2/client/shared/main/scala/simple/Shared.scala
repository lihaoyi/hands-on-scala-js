package simple

case class FileData(name: String, size: Long)

trait Api{
  def list(path: String): Seq[FileData]
}