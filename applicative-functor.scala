import scala.util.{Failure, Success, Try}

implicit class App[A, B](ctor: Try[A => B]) {
  def tryApp(value: Try[A]) =
    (ctor, value) match {
      case (Success(f), Success(v)) => Success(f(v))
      case (Failure(e1), Failure(e2)) =>
        Failure(new Exception(e1.getMessage + ", " + e2.getMessage))
      case (Failure(e1), _) => Failure(e1)
      case (_, Failure(e2)) => Failure(e2)
    }

  def tryAppLazy(value: => Try[A]) =
    ctor match {
      case Failure(exception) => Failure(exception)
      case Success(f) => value.map(f)
    }
}


object Person{
  case class Person private (name: String, age: Int)
  def apply(name: String, age: Int): Try[Person] = {
    Try(Person.curried) tryApp
      tryGetName(name) tryApp
      tryGetAge(age)
  }

  def applyLazy(name: String, age: Int): Try[Person] = {
    Try(Person.curried) tryAppLazy
      tryGetName(name) tryAppLazy
      tryGetAge(age)
  }
}

private def tryGetAge(age: Int): Try[Int] = {
  Try {
    if (age > 0 && age < 100) age
    else throw new Exception(s"$age is not a valid age")
  }
}

private def tryGetName(name: String): Try[String] = {
  val letters = ('a' to 'z') ++ ('A' to 'Z')
  val validChars = letters :+ ' '
  Try {
    if (name.forall(validChars.contains)) name
    else throw new Exception(s"$name is not a valid name")
  }
}

Person("John Doe", 35) //Success(Person(John Doe,35))
Person("John Doe!!!", 35) //Failure(Exception: John Doe!!! is not a valid name)
Person("John Doe", -3) //Failure(Exception: -3 is not a valid age)
Person("John Doe!!!", -3) //Failure(Exception: John Doe!!! is not a valid name, -3 is not a valid age)


Person.applyLazy("John Doe", 150) //Failure(Exception: 150 is not a valid age)
