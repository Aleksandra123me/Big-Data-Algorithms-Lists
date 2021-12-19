import scala.io.Source
import scala.collection.immutable.Seq


@main def MapNode() =
  val filename = "web-Stanford.txt"
  val fileContents = Source.fromFile(filename, "utf-8").getLines.mkString(" ").split("\\W+").map(x => x.toInt)
  //print(fileContents.length)
  var ArrayAllNode1 : Set[Int] = fileContents.toSet
  var ArrayAllNode : List[Int] = fileContents.toList



  var ArrayOfConn : Array[Seq[(Int,Int)]] = Array()
  var i = 0
  while i < (fileContents.length-1) do
    var MySeq : Seq[(Int,Int)] = Seq((fileContents(i), fileContents(i+1)))
    ArrayOfConn=ArrayOfConn.concat(Array(MySeq))
    i = i + 2
  //print(ArrayOfConn.length)

  var MyMap : Array[Seq[(Int, Int, Int)]]= Array()
  MyMap = MapFirst(ArrayOfConn)



  //Zliczanie
  var Map_GroupByKeys = scala.collection.mutable.Map[Int, Array[Seq[(Int, Int, Int)]]]()
  for node <- ArrayAllNode do
    Map_GroupByKeys += (node -> MyMap.filter(x => x(0)(0)==node))  //rozdzielam pary na podstawie pierwszej wartości
  //print(Map_GroupByKeys(1))

  var RESULT: Array[Seq[(Int, Int, Int)]] = Array()
  for (k,v) <- Map_GroupByKeys do
    RESULT=RESULT.concat(Array(reduce_func(k,v)))  //lączę wartości dla każdego nodea w jednego arraya


  //Drukowanie wyników
  print_res(RESULT)


//-------------------------Funkcje-------------------------
def MapFirst(ArrOfSeq : Array[Seq[(Int, Int)]]): Array[Seq[(Int,Int,Int)]] =
  var ReturnMapFirst : Array[Seq[(Int, Int, Int)]]= Array()
  for seq <- ArrOfSeq do
    var TriSeq = Seq[(Int,Int,Int)]()
    var TriSeq2 = Seq[(Int,Int,Int)]()
    TriSeq = Seq((seq(0)(0),0,1))
    TriSeq2 = Seq((seq(0)(1),1,0))
    ReturnMapFirst=ReturnMapFirst.concat(Array(TriSeq))
    ReturnMapFirst=ReturnMapFirst.concat(Array(TriSeq2))
  return ReturnMapFirst


def reduce_func(k: Int, grouped_val: Array[Seq[(Int, Int, Int)]]): Seq[(Int, Int, Int)]=
  var list_of_conn: Array[(Int)] = Array()
  var wynik : Seq[(Int, Int, Int)] = Seq((k,0,0))
  for el <- grouped_val do
    wynik = Seq((k,wynik(0)(1)+el(0)(1),wynik(0)(2)+el(0)(2)))
  return wynik


def print_res(graph: Array[Seq[(Int, Int, Int)]])=
  println("{")
  for el_graph <- graph.clone do
    println(
      "(" + el_graph(0)(0).toString
          + ", inDeg("
          + el_graph(0)(0).toString
          + ") = "
          + el_graph(0)(1).toString
          + " , outDeg("
          + el_graph(0)(0).toString
          + ") = "
          + el_graph(0)(2).toString
          + "),"
    )
  println("}")