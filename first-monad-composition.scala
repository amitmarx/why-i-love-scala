import scala.util.Try

trait Fishy[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B) =
    flatMap(fa)(a => pure(f(a)))

  def tuple[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    flatMap(fa)(a => map(fb)(b => (a, b)))
}

implicit class Composer[F[_], A, B](f: A => F[B]) {
  def ^^^[C](g: B => F[C])(implicit ev: Fishy[F]): A => F[C] = {
    (a: A) => ev.flatMap(f(a))(g)
  }
}

implicit val FishyOption = new Fishy[Option] {
  override def pure[A](a: A) = Some(a)

  override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) =
    fa.flatMap(f)
}

val StringToInt = (i: String) => Try(i.toInt).toOption

val MakeSqare: PartialFunction[Int, Double] = {
  case (i: Int) if i > 0 => Math.sqrt(i)
}

val com = StringToInt ^^^ MakeSqare.lift
Seq[String]("1", "bla", "100", "1000", "-100", "101").map(com).flatten

val v: Option[Int] = FishyOption.flatMap(Some(5))(x => Some(x + 6))

FishyOption.tuple(Some(2), Some(1))

