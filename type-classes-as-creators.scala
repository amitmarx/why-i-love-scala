case class EventA(value1: String, value2: Int, value3: String)
case class EventB(value1: String, value2: Int, value3: String)
case class EventC(value1: String, value2: Int, value3: String, value4: String)

type EventCreator[T] = (String, Int, String) => T

def doSomeLogicAndReturnAnEvent[T]()(implicit eventCreator: EventCreator[T]): Seq[T] = {
  val eventArgs = (1 to 3).map(i => (i.toString, i, i.toString))

  eventArgs.map {
    case (value1, value2, value3) => eventCreator(value1, value2, value3)
  }
}
//functions that know to create events from (String, Int, String)
implicit val eventACreator: EventCreator[EventA] = EventA.apply
implicit val eventBCreator = EventB(_, _, _)//Sugar syntax to: (x:String,y:Int,z:String) => EventB(x,y,z)
implicit val eventCCreator = EventC(_: String, _: Int, _: String, "aConstField")

doSomeLogicAndReturnAnEvent[EventA]() //Seq[EventA] = Vector(EventA(1,1,1), EventA(2,2,2), EventA(3,3,3))
doSomeLogicAndReturnAnEvent[EventB]() //Seq[EventB] = Vector(EventB(1,1,1), EventB(2,2,2), EventB(3,3,3))
doSomeLogicAndReturnAnEvent[EventC]() //Seq[EventC] = Vector(EventC(1,1,1,aConstField), EventC(2,2,2,aConstField), EventC(3,3,3,aConstField))
