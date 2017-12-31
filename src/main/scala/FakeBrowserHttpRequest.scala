import fr.hmil.roshttp.HttpRequest

object FakeBrowserHttpRequest {
  def apply(url: String): HttpRequest = HttpRequest(url)
    .withHeader("User-Agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1")
}
