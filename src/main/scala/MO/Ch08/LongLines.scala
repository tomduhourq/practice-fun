package MO.Ch08

import scala.io.Source
/**
 * Created by tomas on 12/03/15.
 */
object LongLines {

  def processFile(filename: String, width: Int) = {
    // Example shows a definition of a function inside another
    // and how this one profits from the general parameters of the outer one.
    def processLine(line: String) = {
      if(line.length > width)
        println(s"$filename: ${line.trim}")
    }

    val source = Source.fromFile(filename)
    for(line <- source.getLines())
      processLine(line)
  }
}

object FindLongLines {
  def main(args: Array[String]) = {
    val width = args(0).toInt
    for(arg <- args.drop(1))
      LongLines.processFile(arg, width)
  }
}
