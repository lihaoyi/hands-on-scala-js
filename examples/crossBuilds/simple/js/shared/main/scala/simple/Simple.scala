/*shared/main/scala/simple/Simple.scala*/
package simple
object Simple{
  def formatTimes(timestamps: Seq[Long]): Seq[String] = {
    timestamps.map(Platform.format).map(_.dropRight(5))
  }
}