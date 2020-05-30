// library/shared/src/main/scala/simple/Simple.scala
package simple

object Simple {
  def formatTimes(timestamps: Seq[Long]): Seq[String] = {
    timestamps.map(Platform.format(_)).map(_.dropRight(5))
  }
}
