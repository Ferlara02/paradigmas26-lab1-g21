import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {

  type Subscription = (String, String)

  // Lee el archivo JSON y devuelve List[Subscription]
  def readSubscriptions(path: String): List[Subscription] = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    val source = Source.fromFile(path)
    val content = try source.mkString finally source.close()

    val json = parse(content)

    json.extract[List[Map[String, String]]]
      .map(sub => (sub("name"), sub("url")))
  }

  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    source.mkString
  }
}
