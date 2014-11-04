package scalatex.stages

/**
 * Preprocesses the input string to normalize things related to whitespace
 *
 * Find the "first" non-whitespace-line of the text and remove the front
 * of every line to align that first line with the left margin.
 *
 * Remove all trailing whitespace from each line.
 */
object Trim extends (String => String){
  def apply(str: String) = {
    val lines = str.split("\n")
    val offset = lines.iterator
                      .filter(_.length > 0)
                      .next()
                      .takeWhile(_ == ' ')
                      .length
    lines.iterator
         .map(_.drop(offset).replaceFirst("\\s+$", ""))
         .mkString("\n")
  }
}
