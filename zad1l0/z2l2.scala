import scala.io.Source
//import scala.collection.{ Set, mutable }
import scala.collection.mutable.ListBuffer

//scala.collection.mutable.Set[String]
//Robienie shingles
def ShinglesK(book: List[String], k: Int): ListBuffer[String] =
  var list1 = ListBuffer[String]()
  var shingles = ListBuffer[String]()
  for word <- 0 to (book.length-1) do
    list1 += book(word)
  //print(list1)
  //for word <- 1 to (book.length-1) do
  //  list2 += book(word)
  for i <- 0 to (list1.length-k) do
    var myshingle = String()
    for j <- 0 to (k-1) do
      if (j==0) then
        myshingle = list1(i+j)
      else
        myshingle = myshingle + (" ") + list1(i+j)
    shingles += myshingle
  //print(shingles)
  return shingles



@main def JaccardSimilarity(bookA: String,bookB: String, k:Int) =
  val stopwords=Source.fromFile("C:\\Users\\Aleksandra\\Desktop\\PWr\\Big Data Analytics\\Semestr 2\\BDA\\zad1l0\\project\\stop_words_english.txt","utf-8").getLines.toList
  //bookA
  var fileA=Source.fromFile(bookA,"utf-8").getLines.mkString(" ").split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_)).toList
  //bookB
  var fileB=Source.fromFile(bookB,"utf-8").getLines.mkString(" ").split("\\W+").map(x => x.toLowerCase).filter(!stopwords.contains(_)).toList
  var shinglesA  =  Set[String]()
  var shinglesB  =  Set[String]()

  shinglesA = ShinglesK(fileA,k).toSet
  shinglesB = ShinglesK(fileB,k).toSet
  //print(fileB)
  //print(shinglesA, shinglesB)
  //A u B
  var sumAB  =  Set[String]()
  sumAB=shinglesA.union(shinglesB)
  val len_sumAB: Int = sumAB.size
  println(len_sumAB)
  //print(len_sumAB)

  //A and B
  var intersection  =  Set[String]()
  intersection = shinglesA & shinglesB
  val len_intersection: Int = intersection.size
  println(len_intersection)
  //Jaccard Sim

  val JaccardFull : Float = len_intersection/len_sumAB
  println(JaccardFull)










