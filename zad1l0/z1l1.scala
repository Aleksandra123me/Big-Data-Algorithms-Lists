import scala.io.Source
import java.util.regex.Pattern
import java.io._
@main def WordCloudExtended() =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  val file="(-file .*txt)".r
  val pfirst="(-print [\\d]+)".r
  val csv="(-csv [\\d]+)".r
  val tdidf="(-tdidf [\\d]+)".r
  // create an empty map
  var WordCountMAP = scala.collection.mutable.Map[String, Int]()
  //var WordCountMAP: scala.collection.mutable.Map[String, Int] = Map()
  for (l <- io.Source.stdin.getLines) do
    l match
      case file(l) =>
        var str1 = l.substring(6,l.length)
        var file=Source.fromFile(str1,"utf-8").getLines.flatMap(_.split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_))).toList
//otwieram plik, usuwam stopwordsy, zmieniam na male litery i tworzę listę...
        //for(i <- stopwords)
          //if (file.contains(i)) then
            //file=file.filterNot(el => el == i)
        for
          word <- file
        do
          if WordCountMAP.contains(word) then   //jeżeli slowo jest w slowniku, to zwiekszam wartosc o 1.....
            WordCountMAP += (word -> (WordCountMAP(word)+1))
          else
            WordCountMAP += (word -> 1)


      //Tu robię TF.IDF
        var WordAndTDIDF = scala.collection.mutable.Map[String, Double]()
        var WordTDIDFMap = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]]()


        var file2=Source.fromFile(str1,"utf-8").getLines.mkString.split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_)).toList

        var cleanFile_len = file2.length//liczba wszytskich słów w danym pliku
        //println(cleanFile_len)
        //println(Book_nostopwords)
        for
          word <- file2
        do
          WordAndTDIDF += (word -> ((file2.count(_ == word)).toDouble/cleanFile_len.toDouble))
          WordTDIDFMap += (str1 -> WordAndTDIDF)
        println(WordTDIDFMap)
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



