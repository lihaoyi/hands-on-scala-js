/*shared/main/scala/simple/Simple.scala*/
package simple
object Simple{
  def formatTimestamps(timestamps: Seq[Long]) = {
    timestamps.map(Platform.format)
              .mkString("\n")
    }
  }
}