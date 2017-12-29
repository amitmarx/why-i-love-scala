import org.joda.time.DateTime

trait Event
trait Criteria

case class DiscountEvent(discount: Double) extends Event
case class NotesChanged(notes: String) extends Event
case class Expired(now: DateTime) extends Event

trait Discountable[T] {
  def withDiscount(discount: Double): T
}

trait Expireable[T] {
  def withExpirationDate(date: DateTime): T
}

case class Invoice(discount: Double, dueDate: Option[DateTime] = None)
  extends Discountable[Invoice] with Expireable[Invoice] {
  override def withDiscount(discount: Double): Invoice = copy(discount = discount)
  override def withExpirationDate(date: DateTime): Invoice = copy(dueDate = Some(date))
}

case class PriceQuote(discount: Double) extends Discountable[PriceQuote] {
  override def withDiscount(discount: Double): PriceQuote = copy(discount = discount)
}

def handleDiscountEvent[T]: PartialFunction[(Event, T), T] = {
  case (DiscountEvent(discount), discountable: Discountable[T]) =>
    discountable.withDiscount(discount)
}

def handleExpiration[T]: PartialFunction[(Event, T), T] = {
  case (Expired(at), expireable: Expireable[T]) =>
    expireable.withExpirationDate(at)
}

def fallback[T]: PartialFunction[(Event, T), T] = {
  case (_, x) => x
}

def eventHandler[T] =
  handleDiscountEvent[T] orElse
    handleExpiration[T] orElse
    fallback[T]

val invoice = Invoice(0)
val priceQuote = PriceQuote(0)

val events = Seq(DiscountEvent(10), NotesChanged("hello"), Expired(DateTime.now()))

events.foldLeft(invoice) { (acc, event) =>
  eventHandler((event, acc))
}

events.foldLeft(priceQuote) { (acc, event) =>
  eventHandler((event, acc))
}

