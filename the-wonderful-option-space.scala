def toIntOption(value: String): Option[Int] = {
  try {
    Some(value.toInt)
  } catch {
    case e: Exception => None
  }
}


def divide(a: Int, b: Int): Option[Int] = {
  (a,b) match {
    case (_,0) => None
    case _ => Some(a/b)
  }
}


def plus(a: Int, b: Int): Option[Int] = {
  Some(a+b)
}


val v = "100,50,-100"

v.split(",").map(toIntOption).flatMap(_.map(plus(_,100))).
  fold(Some(1))((acc,value)=>{
    (acc,value) match {
      case (Some(a: Int), Some (b: Int)) => divide (a, b)
      case _=> None
    }
  })

