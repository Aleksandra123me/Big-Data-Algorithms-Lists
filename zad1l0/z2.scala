import scala.collection.immutable._

@main def zad2() =
  var y :  Array[Seq[(Int, Array[Int])]] = Array(Seq((1,Array(2,3))), Seq((3,Array(1,5))), Seq((2,Array(5))), Seq((5, Array.emptyIntArray)))
  //val w = map_func(Seq((1,Array(2,3))))
  var inverted_kv: Array[Seq[(Int, Array[Int])]]=Array()
  var ArrayAllNode: Array[(Int)] = Array() //lista wszytskich nodes
  for seq <- y do  //po każdej parze (klucz, wartość)
    var wynik_map_func = map_func(seq)
    ArrayAllNode= ArrayAllNode.concat(Array(seq(0)(0)))  //lista wszytskich nodes
    inverted_kv=inverted_kv.concat(wynik_map_func) //pary odwrócone (wartość, klucz)
  var Map_GroupByKeys = scala.collection.mutable.Map[Int, Array[Seq[(Int, Array[Int])]]]()
  for node <- ArrayAllNode do
    Map_GroupByKeys += (node -> inverted_kv.filter(x => x(0)(0)==node))  //rozdzielam pary na podstawie pierwszej wartości

  var Inverted_RESULT: Array[Seq[(Int, Array[Int])]] = Array()
  for (k,v) <- Map_GroupByKeys do
    Inverted_RESULT=Inverted_RESULT.concat(Array(reduce_func(k,v)))  //lączę wartości dla każdego nodea w jednego arraya
  //Drukowanie wyników

  print_res(y)
  print_res(Inverted_RESULT)
  //for z <- ArrayAllNode do
    //println(z)
//for www <- w do
 // println(www(0))
def map_func(conn_node: Seq[(Int, Array[Int])]): Array[Seq[(Int, Array[Int])]] =
  var inverted_conn_all: Array[Seq[(Int, Array[Int])]]=Array()
  for n <- conn_node(0)(1) do
    var inverted_conn: Seq[(Int, Array[Int])] = Seq((n,Array(conn_node(0)(0))))
    inverted_conn_all=inverted_conn_all.concat(Array(inverted_conn))
    //println(inverted_conn_all.length + " " + n.toString)
  return inverted_conn_all
def reduce_func(k: Int, grouped_val: Array[Seq[(Int, Array[Int])]]):Seq[(Int, Array[Int])]=
  var list_of_conn: Array[(Int)] = Array()
  for el <- grouped_val do
    list_of_conn=list_of_conn.concat(el(0)(1))
  var list_of_conn_withKey: Seq[(Int, Array[Int])] =Seq((k,list_of_conn))
  println("reducefunc " + list_of_conn_withKey)
  return list_of_conn_withKey
def print_res(graph: Array[Seq[(Int, Array[Int])]])= {
  println("[")
  for el_graph <- graph.clone.dropRight(1) do
    println("("+el_graph(0)(0).toString + ", ["+ el_graph(0)(1).mkString(", ") +"]),")
  println("("+graph.takeRight(1)(0)(0)(0).toString + ", ["+ graph.takeRight(1)(0)(0)(1).mkString(", ") +"])")
  println("]")
}



//def red_funk(Pairs_mapf: Array[Seq[(Int, Array[Int])]]):
