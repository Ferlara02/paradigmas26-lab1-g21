object Main {

    //Utilizamos Set en vez de List para hacer búsquedas en O(1) en vez de O(n)
    val stopwords: Set[String] = Set("the", "about", "above", "after", "again", "against", "all", "am", "an",
    "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
    "before", "being", "below", "between", "both", "but", "by", "can't",
    "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
    "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
    "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
    "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
    "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
    "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
    "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
    "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
    "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
    "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
    "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
    "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
    "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
    "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
    "what's", "when", "when's", "where", "where's", "which", "while", "who",
    "who's", "whom", "why", "why's", "with", "won't", "would",
    "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
    "yourself", "yourselves"
    )

  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    //getOrElse(List.empty) devuelve lista vacía si se produce fallo
    val subscriptions: List[(String, String)] = FileIO.readSubscriptions("subscriptions.json").getOrElse(List.empty)

    val allPosts: List[(String, List[FileIO.Post])] = subscriptions.map { tupla =>
      val name = tupla._1
      val url  = tupla._2

      println(s"Fetching posts from: $name")
      val posts = FileIO.downloadFeed(url).getOrElse(List.empty)
      val filtered_post = posts.filter{ case (subreddit,title, selftext, date, score, url) =>
        title.trim.nonEmpty && selftext.trim.nonEmpty 
        /*
          trim fitlra casos "solo espacios" al convertirlos en "" (casos vacíos)
          y nonEmpty filtra casos vacíos
        */
      }.sortBy(-_._5) // Ordena por score descendente para que los "primeros" sean los más votados

      (name, filtered_post)
    }

    // Calculamos la frecuencia de palabras por subreddit
    val frequenciesBySubscription: Map[String, Seq[(String, Int)]] =
      FileIO.wordsFrequency(allPosts, stopwords).toMap

    //Elije los 10 términos más frecuentes de cada subreddit y los muestra junto con el total de score y los 5 posts más votados
    def formatTopPosts(posts: List[FileIO.Post]): String =
      posts.take(5).zipWithIndex.map { case ((_, title, _, date, _, url), idx) =>
        s"${idx + 1}. $title\n  Date: $date\n  URL: $url"
      }.mkString("\n")

    //generamos la salida para cada subreddit con su total de score, palabras frecuentes y top posts
    val output = allPosts.map { case (name, posts) =>
      val totalScore = FileIO.totalScore(posts)
      val topWords = frequenciesBySubscription.getOrElse(name, Seq.empty).take(10)
      val topPosts = formatTopPosts(posts)

      s"""  |${"=" * 40}
          |Subscription: $name
          |${"=" * 40}
          |
          |Total score: $totalScore
          |
          |Frequent words: $topWords
          |
          |Top 5 posts:
          |$topPosts
          |""".stripMargin
    }.mkString("\n")

    println(header)
    println(output)  
  }
}