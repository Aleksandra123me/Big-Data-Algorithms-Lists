import scala.io.Source
import java.util.regex.Pattern
@main def WordCloudExtended() =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  val file="(-file .*txt)".r
  // create an empty map
  var WordCountMAP = scala.collection.mutable.Map[String, Int]()
  //var WordCountMAP: scala.collection.mutable.Map[String, Int] = Map()
  for (l <- io.Source.stdin.getLines) do
    l match
      case file(l) =>
        var str1 = l.substring(6,l.length)
        var file=Source.fromFile(str1,"utf-8").getLines.flatMap(_.split("\\W+")).toList
        for(i <- stopwords)
          if (file.contains(i)) then
            file=file.filterNot(el => el == i)
        for (word <- file)
          if WordCountMAP.contains(word) then
            WordCountMAP += (word -> (WordCountMAP(word)+1))
          else
            WordCountMAP += (word -> 1)
      case _ =>
        var WrittenText = l.flatMap(_.split("\\W+")).toList
        for(i <- stopwords)
          if (WrittenText.contains(i)) then
            WrittenText=WrittenText.filterNot(el => el == i)
        for (word <- WrittenText)
          if WordCountMAP.contains(word) then
            WordCountMAP += (word -> (WordCountMAP(word)+1))
          else
            WordCountMAP += (word -> 1)
          println(WordCountMAP)

