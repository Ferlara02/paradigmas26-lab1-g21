import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {

  type Subscription = (String, String)
  type Post = (String, String, String) // (subreddit, title, selftext)

  // Lee el archivo JSON y devuelve List[Subscription]
  def readSubscriptions(path: String): Option[List[Subscription]] = {
    //Option para devolver None en caso de error
    try {
      implicit val formats: DefaultFormats.type = DefaultFormats

      val source = Source.fromFile(path)
      val content = try source.mkString finally source.close()

      val json = parse(content)

      json.extract[List[Map[String, String]]]
        .map(sub => (sub("name"), sub("url")))
    }catch {
      case e: Exception =>
        None
    }
    

  // Obtener la lista de posts
  def downloadFeed(url: String): List[Post] = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    val source = Source.fromURL(url)
    val content = try source.mkString finally source.close()

    val json = parse(content)

    // Obtengo la lista de posts
    val children = (json \ "data" \ "children").children

    children.map { child =>
      val data = child \ "data"
      
      // Extraigo campos
      val subreddit = (data \ "subreddit").extract[String]
      val title     = (data \ "title").extract[String]
      val selftext  = (data \ "selftext").extract[String]
      
      // Retornamos la tupla con el tipo Post: (String, String, String, String, Int)
      (subreddit, title, selftext)
    }
  }
}
