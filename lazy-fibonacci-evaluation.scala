def fibo(): Stream[Int] = {
  def fibo(current: Stream[Int]): Stream[Int] = {
    val nextItem = current.take(2).sum
    println(s"fibo number ${current.length-2} was calculated")
    val stream = nextItem #:: current
    nextItem #:: fibo(nextItem #:: current)
  }
  fibo(Stream(1, 1))
}


val lazyFiboEvaluator=fibo()

val fibo10 = lazyFiboEvaluator(10)
println(s"The 10th fibo is:${fibo10}")
val fibo3 = lazyFiboEvaluator(3)
println(s"The 3rd fibo is:${fibo3}")
val fibo12 = lazyFiboEvaluator(12)
println(s"The 12th fibo is:${fibo12}")


