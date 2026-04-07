object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    //getOrElse(List.empty) devuelve lista vacía si se produce fallo
    val subscriptions: List[(String, String)] = FileIO.readSubscriptions("subscriptions.json").getOrElse(List.empty)

    val allPosts: List[(String, List[FileIO.Post])] = subscriptions.map { tupla =>
      val name = tupla._1
      val url  = tupla._2

      println(s"Fetching posts from: $name")
      val posts = FileIO.downloadFeed(url).getOrElse(List.empty)
      val filtered_post = posts.filter{ case (subreddit,title,selftext) =>
        title.trim.nonEmpty && selftext.trim.nonEmpty 
        /*
          trim fitlra casos "solo espacios" al convertirlos en "" (casos vacíos)
          y nonEmpty filtra casos vacíos
        */
      }
      (name, filtered_post)
    }

    val output = allPosts
      .map { case (name, posts) => Formatters.formatSubscription(name, posts) }
      .mkString("\n")

    println(output)   
  }
}

/* Test hardcodeado para fitlrado

val testPosts: List[FileIO.Post] = List(
      ("scala", "Post con titulo y descripcion", "selftext completo"),
      ("scala", "", "selftext completo"),          // titulo vacío
      ("scala", "Post con titulo", ""),            // selftext vacío
      ("scala", "   ", "   "),                     // solo espacios
      ("scala", "Post normal", "descripcion normal")
    )

    val filteredPosts = testPosts.filter { case (subreddit, title, selftext) =>
      title.trim.nonEmpty && selftext.trim.nonEmpty
    }

    println(filteredPosts) 
*/