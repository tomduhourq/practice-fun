import scala.concurrent.{Await, ExecutionContext, Future}
def download(url: String)(implicit ec: ExecutionContext): Future[String] =
  Future{ "Awesome content from " + url }

def downloadAll(urls: Seq[String])(implicit ec: ExecutionContext): Future[Seq[String]] = {
  urls.foldLeft(Future.successful(Seq.empty[String])) {
    (acumFutures, elem) => for (seq <- acumFutures ; down <- download(elem)) yield seq ++ Seq(down)
  }
  //  Future.sequence(urls map download)
}

import scala.concurrent.ExecutionContext.Implicits.global
val fut = downloadAll(Seq("google", "spotify", "cloudera"))

import scala.concurrent.duration._
Await.result(fut, 1 seconds)