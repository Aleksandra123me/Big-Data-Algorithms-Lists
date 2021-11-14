
import scala.io.Source

@main def WordCloud() =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  var file=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 1\\Programming and Classification\\Lista 2\\catch22.txt","utf-8").getLines.split("\\W+").toList
  //usuwanie stopwordsów
  for(i <- stopwords)
    if (file.contains(i))
      file=file.filterNot(el => el == i)
  var WordNum = file.map(x => (x, file.count(_ == x)))
  println(WordNum)


