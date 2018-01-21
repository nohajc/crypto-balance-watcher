package cz.nohajan.cryptowatcher

import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.exceptions.HttpException
import fr.hmil.roshttp.response.SimpleHttpResponse
import org.scalatra._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object FakeBrowserHttpRequest {
  def apply(url: String): HttpRequest = HttpRequest(url)
    .withHeader("User-Agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1")
}


class MyScalatraServlet extends ScalatraServlet with FutureSupport {
  protected implicit def executor: ExecutionContext = global

    get("/") {
    views.html.index()
  }

  get("/proxy") {
    val url = params("url")
    val fwdHeaderNames = Array("Accept", "X-MBX-APIKEY")

    val fwdHeaders = for {
      n <- fwdHeaderNames
      v <- Option(request.getHeader(n))
    } yield n -> v

    log(s"proxy request to $url")

    val fwd = FakeBrowserHttpRequest(url).withHeaders(fwdHeaders:_*)

    new AsyncResult() {
      override val is: Future[_] = fwd.send().map(_.body).recover {
        case HttpException(e: SimpleHttpResponse) =>
          response.setStatus(e.statusCode)
          ""
      }
    }
  }
}
