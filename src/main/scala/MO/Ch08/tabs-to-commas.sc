import java.io.{File, PrintWriter}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

def filesInDir(dir: String): List[File] = {
  val d = new File(dir)
  if (d.exists && d.isDirectory)
    d.listFiles
      .filter(f => f.isFile && f.getName.endsWith("csv"))
      .toList
  else List.empty[File]
}

val dir = "dir"

// get files metadata
val csvFiles = filesInDir(dir)

// prepare writers to write in parallel
val printWriters = csvFiles.map { f =>
  val file = new File(s"${dir}d${f.getName}")
  new PrintWriter(file)
}

// start executing in parallel
val futOperations: List[Future[PrintWriter]] = csvFiles zip printWriters map { case (file, writer) =>
  Future {
    println(s"Running pipe on ${file.getName}")
    io.Source
      .fromFile(file)
      .getLines
      .map(_.replace("\t", ","))
      .foreach(commaSeparatedLine => writer.write(commaSeparatedLine + "\n"))
    writer
  }
}

Future.sequence(futOperations).onComplete {
  case Success(writers) => writers.foreach(_.close)
  case Failure(ex) => println(ex.getMessage)
}



