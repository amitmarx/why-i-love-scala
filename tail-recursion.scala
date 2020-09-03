def fact(n: Int): Int =
  if (n == 0){
    (new RuntimeException).getStackTrace.foreach(println)
    1
  }
  else n * fact(n - 1)

def tailFact(n: Int, result: Int): Int =
  if (n == 0) {
    (new RuntimeException).getStackTrace.foreach(println)
    result
  }
  else tailFact(n - 1, result * n)


tailFact(4,1)
println("------------------------")
fact(4)

/*
$line24.$read$$iw$$iw$.tailFact(<console>:13)
$line25.$read$$iw$$iw$.<init>(<console>:13)
$line25.$read$$iw$$iw$.<clinit>(<console>)
$line25.$eval$.$print$lzycompute(<console>:7)
$line25.$eval$.$print(<console>:6)
$line25.$eval.$print(<console>)
res0: Int = 24
------------------------
$line23.$read$$iw$$iw$.fact(<console>:13)
$line23.$read$$iw$$iw$.fact(<console>:16)
$line23.$read$$iw$$iw$.fact(<console>:16)
$line23.$read$$iw$$iw$.fact(<console>:16)
$line23.$read$$iw$$iw$.fact(<console>:16)
$line27.$read$$iw$$iw$.<init>(<console>:13)
$line27.$read$$iw$$iw$.<clinit>(<console>)
$line27.$eval$.$print$lzycompute(<console>:7)
$line27.$eval$.$print(<console>:6)
$line27.$eval.$print(<console>)
res2: Int = 24
 */
