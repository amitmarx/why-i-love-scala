trait A{
  def message:String
  def allMessage: Seq[String] = Seq(message)
}

class B(msg: String) extends A {
  override def message: String = msg
}

trait C extends A{
  val otherMessage = "foo"

  override def allMessage = super.allMessage :+ otherMessage

}

val withoutMixin = new B("baz")
val withMixin = new B("baz") with C

withoutMixin.allMessage // List(baz)
withMixin.allMessage // List(baz, foo)
