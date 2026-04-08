import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter

object TextProcessing {
  def formatDateFromUTC(createdUtc: Long): String = {
    val instant = Instant.ofEpochSecond(createdUtc)
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault())
    formatter.format(instant)
  }
}

object FileIO {

  type Subscription = (String, String)
  type Post = (String, String, String, String, Int, String) // (subreddit, title, selftext, date, score, url)

  // Lee el archivo JSON y devuelve List[Subscription]
  def readSubscriptions(path: String): Option[List[Subscription]] = {
    //Option para devolver None en caso de error
    try {
      implicit val formats: DefaultFormats.type = DefaultFormats

      val source = Source.fromFile(path)
      val content = try source.mkString finally source.close()

      val json = parse(content)

      Some(json.extract[List[Map[String, String]]]
        .map(sub => (sub("name"), sub("url"))))
    }catch {
      case e: Exception =>
        None
    }
  }
    

  // Obtener la lista de posts
  def downloadFeed(url: String): Option[List[Post]] = {
    try {
      implicit val formats: DefaultFormats.type = DefaultFormats

      val source = Source.fromURL(url)
      val content = try source.mkString finally source.close()

      val json = parse(content)

      // Obtengo la lista de posts
      val children = (json \ "data" \ "children").children

      Some{ children.map { child =>
        val data = child \ "data"
      
        // Extraigo campos
        val subreddit = (data \ "subreddit").extract[String]
        val title     = (data \ "title").extract[String]
        val selftext  = (data \ "selftext").extract[String]
        val score     = (data \ "score").extract[Int]
        val url       = (data \ "url").extract[String]
      
        // Retornamos la tupla con el tipo Post: (String, String, String, String, Int)

        val createdUtc = (data \ "created_utc").extract[Double].toLong
        val date = TextProcessing.formatDateFromUTC(createdUtc) // Convertir a String para mantener el tipo Post


        (subreddit, title, selftext, date, score, url)
      }
      }
    } catch {
      case e: Exception =>
        None
    }
  }
  
  // Ejercicio 5
  // stopwords se pasan como parametros para permitir agregar o quitar palabras facilmente
  def wordsFrequency(allPosts: List[(String, List[Post])], stopwords: Set[String]) : List[(String,Seq[(String,Int)])] = {

    /*De cada lista de post extraemos las palabras y las guardamos como 
    (subreddit, lista de palabras)*/
    val wordsPerSubreddit = allPosts.map{
      case (subreddit, postsList) => 
        //Extraemos todas las palabras de los posts
        val upperWords = postsList.flatMap{
          case (_, _, selftext, _, _, _) =>
            selftext.split("[,*/:'’.)\\(!?\\s)]+").filter( word => 
              word.nonEmpty && 
              word.head.isUpper && // Si va primero y llega string vacía salta excepción
              !stopwords.contains(word.toLowerCase)
              )
        }
        (subreddit, upperWords)
    }

    /* Contamos las palabras de cada subrredit 
    Ahora el tipo será List[(String,Map[String,Int])]*/
    val subredditFrequencies = wordsPerSubreddit.map{
      case (subreddit, wordList) =>
        /*Grupos de de cada palabra y sus veces repetidas*/
        val wordGroups = wordList.groupBy(identity) 
        /*Mapa de frecuencias Map("palabra" -> Int de repeticiones)*/
        val frequencies = wordGroups.map { 
          case (word, frequency) =>
            (word, frequency.size)
        }
        (subreddit, frequencies)
    }

    /* Por último ordenamos de mayor a menor las frecuencias de cada subreddit
    Ahora el tipo será List[(String,Seq[String,Int])]*/
    val sortedFrequencies = subredditFrequencies.map{
      case (subreddit, frequencies) =>
        /* Para usar sortBy primero transformo mapa */
        val frequenciesSorted = frequencies.toSeq.sortBy(t => -t._2)
        (subreddit, frequenciesSorted)
    }

    sortedFrequencies

    /* Uso del resultado de wordsFrequency:
      Devuelve List[(subreddit, Seq[(palabra, frecuencia)])] ordenado de mayor a menor.
      Ejemplo de acceso:
        val result = wordsFrequency(allPosts, stopwords)
        result.find(_._1 == "Scala")        // busca el subreddit "Scala"
          .map(_._2.take(10))               // toma las 10 palabras más frecuentes
    */
  }

  //Calcula el score total de una lista de posts
  def totalScore(posts: List[FileIO.Post]): Int = {
    posts.foldLeft(0){ (acc, post) =>
    acc + post._5 // score es el quinto elemento de la tupla Post
    }
  }


}
