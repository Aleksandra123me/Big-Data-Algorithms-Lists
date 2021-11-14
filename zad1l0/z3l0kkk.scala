import scala.io.Source
import java.util.regex.Pattern
import java.io._
@main def WordCloudExtended() =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  val file = "(-file .*txt)".r
  val pfirst = "(-print [\\d]+)".r
  val csv = "(-csv [\\d]+)".r
  // create an empty map
  var WordCountMAP = scala.collection.mutable.Map[String, Int]()
  //var WordCountMAP: scala.collection.mutable.Map[String, Int] = Map()
  for (l <- io.Source.stdin.getLines) do
    l match
      case file(l) =>
        var str1 = l.substring(6,l.length)
        var file=Source.fromFile(str1,"utf-8").getLines.flatMap(_.split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_))).toList
        //for(i <- stopwords)
          //if (file.contains(i)) then
            //file=file.filterNot(el => el == i)
        for
          word <- file
        do
          if WordCountMAP.contains(word) then
            WordCountMAP += (word -> (WordCountMAP(word)+1))
          else
            WordCountMAP += (word -> 1)
      case pfirst(l) =>
        val cnt = l.substring(7,l.length).toInt
        val SortMap = WordCountMAP.toSeq.sortWith(_._2 > _._2).take(cnt)
        println(SortMap)
      case csv(l) =>
        val cnt = l.substring(5,l.length).toInt
        val SortMap = WordCountMAP.toSeq.sortWith(_._2 > _._2).take(cnt)
        val FileCount = new PrintWriter(new File("TopNWords.csv"))
        for (k, v) <- SortMap do FileCount.write(s"$v,$k\n")
        FileCount.close
      case _ =>
        var WrittenText = l.split("\\W+").toList
        for(i <- WrittenText)
          if (stopwords.contains(i.toLowerCase)) then
            WrittenText=WrittenText.filterNot(el => el == i)
        for (word <- WrittenText)
          if WordCountMAP.contains(word.toLowerCase) then
            WordCountMAP += (word.toLowerCase -> (WordCountMAP(word.toLowerCase)+1))
          else
            WordCountMAP += (word.toLowerCase -> 1)


