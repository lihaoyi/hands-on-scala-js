/*shared/main/scala/simple/Simple.scala*/
package simple
object Simple{
  def formatTimes(timestamps: Seq[Long]): String = {
    timestamps.map(Platform.format).mkString("\n")
  }
}