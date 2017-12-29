case class EventA(value1: String, value2: Int, value3: String)
case class EventB(value1: String, value2: Int, value3: String)

type EventCreator[T] = (String, Int, String) => T

def foo[T](implicit creator: EventCreator[T]): Seq[T] = {
  val args = (1 to 10).map(i => (i.toString, i, i.toString))
  args.map {
    case (value1, value2, value3) => creator(value1, value2, value3)
  }
}

implicit val eventACreator: EventCreator[EventA] = EventA.apply

foo[EventA]

implicit val eventBCreator = EventB(_, _, _)
foo[EventB]

