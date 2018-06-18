def allSubs(stream: Stream[Int]): Stream[Seq[Int]] = {
  def innerSub(itemStream: Stream[Int], currentStage: Seq[Seq[Int]], nextStage: Seq[Seq[Int]]): Stream[Seq[Int]] = {
    (itemStream, currentStage) match {
      case (Stream.Empty, _) => Stream.empty[Seq[Int]]
      case (_, Seq()) => innerSub(itemStream.tail, nextStage, Seq.empty[Seq[Int]])
      case (item #:: _, list :: listTail) =>
        (Seq(item) ++ list) #:: innerSub(itemStream, listTail, nextStage ++ Seq(list, Seq(item) ++ list))
    }
  }
  innerSub(stream, Seq(Seq.empty[Int]), Seq.empty[Seq[Int]])
}
def from(start: Int): Stream[Int] = start #:: from(start + 1)

val stream = from(0).take(3)

val output = allSubs(stream)

output.foreach(println)

//val seq = Seq(1,2)
//
//seq match{
//  case (head::tail)=>println(tail)
//}


//val v1:Stream[Int] = 1 #:: 2 #:: Stream.empty[Int]
//val v2:Stream[Int]= 4 #:: 52 #:: Stream.empty[Int]
//
//(v1#:::v2).take(10).foreach(println)



//def allSubs(stream: Stream[Int]): Stream[Seq[Int]] = {
//  def innerSub(itemStream: Stream[Int], currentStage: Seq[Seq[Int]], nextStage: Seq[Seq[Int]]): Stream[Seq[Int]] = {
//    if (itemStream.isEmpty) {
//      return
//    }
//    if (currentStage.isEmpty) {
//      return innerSub(itemStream.tail, nextStage, Seq.empty[Seq[Int]])
//    }
//    val item = itemStream.head
//    val list = currentStage.head
//    list #:: (Seq(item) ++ list) #:: innerSub(itemStream, currentStage.tail, nextStage ++ Seq(list, Seq(item) ++ list))
//  }
//
//  innerSub(stream, Seq(Seq.empty[Int]), Seq.empty[Seq[Int]])
//}
