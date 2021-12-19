import scala.io.Source
import java.util.regex.Pattern
import java.io._
import scala.math._

//w ilu tekstach wystepuje dane slowo
def WordInTexts(MapOfMaps: scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]], word: String): Int =
  var Text_contains = 0
  for (k,v) <- MapOfMaps do
    if v.contains(word) then
      Text_contains = Text_contains+1
  return Text_contains


@main def WordCloudExtended() =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  var FileWordCntMap = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]]()
  var TDIDF_ALLTxt = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]]()
  val file = "(-file .*txt)".r
  val pfirst = "(-print [\\d]+)".r
  val pkt_b = "(-pkt_b [\\d]+)".r
  val pkt_c = "(-pkt_c [\\d]+)".r
  val pkt_d = "(-pkt_d [\\d]+)".r
  // create an empty map
  var WordCountMAP = scala.collection.mutable.Map[String, Int]()
  var NumberOfWord_Txt = scala.collection.mutable.Map[String, Int]()
  //var WordCountMAP: scala.collection.mutable.Map[String, Int] = Map()
  for (l <- io.Source.stdin.getLines) do
    l match
      case file(l) =>
        var str1 = l.substring(6,l.length)
        var file=Source.fromFile(str1,"utf-8").getLines.mkString(" ").split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_)).toList
        //println(file)
        NumberOfWord_Txt += (str1 -> file.length) //map z liczbą wyrazów dla każdego
        var WordCntMap = scala.collection.mutable.Map[String, Int]()

        for
          word <- file
        do
          WordCntMap += (word -> (file.count(_ == word)))  //dla tekstu
          if WordCountMAP.contains(word) then  //dla wszytskich tekstow razem
            WordCountMAP += (word -> (WordCountMAP(word)+1))
          else
            WordCountMAP += (word -> 1)
        WordCntMap = WordCntMap.-("")//usuwam spację z tej listy


        FileWordCntMap += (str1 -> WordCntMap)
        //println(WordCountMAP)
        //println(FileWordCntMap)
        //println(NumberOfWord_Txt)
      case pkt_b(l) =>
        var cnt = l.substring(7,l.length).toInt
        var AllWordsMap = FileWordCntMap.clone
        AllWordsMap += ("all" -> WordCountMAP)
        var mapOfSeq = scala.collection.mutable.Map[String, Seq[(String, Int)]]()
        for (k,v) <- AllWordsMap do
          mapOfSeq(k) = v.toSeq.sortWith(_._2 > _._2).take(cnt)
        println(mapOfSeq)
        //println(FileWordCntMap)
      case pkt_c(l) =>
        for (file, word_map) <- FileWordCntMap do
          var TDIDF_ONETxt = scala.collection.mutable.Map[String, Double]()
          for (word, number) <- word_map do
            val loop_tf: Double = ((number).toDouble/(NumberOfWord_Txt(file)).toDouble).toDouble
            //println(loop_tf)
            //println(WordInTexts(FileWordCntMap, word))
            val loop_idf: Double = log((FileWordCntMap.size).toDouble/(WordInTexts(FileWordCntMap, word)).toDouble)
            //println(loop_idf)
            TDIDF_ONETxt += (word -> loop_tf*loop_idf)
          TDIDF_ALLTxt += (file -> TDIDF_ONETxt)
        //println(TDIDF_ALLTxt)


        var cnt = l.substring(7,l.length).toInt
        var AllTexts_TDIDF = TDIDF_ALLTxt.clone

        var mapOfSeq2 = scala.collection.mutable.Map[String, Seq[(String, Double)]]()
        for (k,v) <- AllTexts_TDIDF do
          mapOfSeq2(k) = v.toSeq.sortWith(_._2 > _._2).take(cnt)
        println(mapOfSeq2)

      case pfirst(l) =>
        val cnt = l.substring(7,l.length).toInt
        val SortMap = WordCountMAP.toSeq.sortWith(_._2 > _._2).take(cnt)
        println(SortMap)




