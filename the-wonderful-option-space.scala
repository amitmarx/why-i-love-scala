def toDoubleOption(value: String): Option[Double] = {
  try {
    Some(value.toDouble)
  } catch {
    case e: Exception => None
  }
}


def divide(a: Double, b: Double): Option[Double] = {
  (a,b) match {
    case (_,0) => None
    case _ => Some(a/b)
  }
}


def plus(a: Double, b: Double): Option[Double] = {
  Some(a+b)
}

def Add100EachAndThenFoldDivide(str:String) = {
  val splitedValues = str.split(",")
  val add100function = plus(_, 100)

  val convertedValues = splitedValues.map(toDoubleOption)
  val valuesIncreasedBy100 = convertedValues.map(_.flatMap(add100function))
  val foldDivideValue = valuesIncreasedBy100.fold(Some(1.0))((acc, value) => {
    (acc, value) match {
      case (Some(a: Double), Some(b: Double)) => divide(a, b)
      case _ => None
    }
  })
  foldDivideValue
}
Add100EachAndThenFoldDivide("100,50,1000,60,64.5") //Some(1.1513309385649811E-12)
Add100EachAndThenFoldDivide("100,50,1000,BLA,60,64.5") //res1: Option[Double] = None
Add100EachAndThenFoldDivide("100,50,-100") //res1: Option[Double] = None
