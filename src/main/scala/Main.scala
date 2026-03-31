object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    val subscriptions: List[(String, String)] = FileIO.readSubscriptions("subscriptions.json")

    val allPosts: List[(String, List[FileIO.Post])] = subscriptions.map { tupla =>
      val name = tupla._1
      val url  = tupla._2

      println(s"Fetching posts from: $name")
      val posts = FileIO.downloadFeed(url)
      (name, posts)
    }

    val output = allPosts
      .map { case (name, posts) => Formatters.formatSubscription(name, posts) }
      .mkString("\n")

    println(output)
  }
}
