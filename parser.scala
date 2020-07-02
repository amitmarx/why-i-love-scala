sealed trait MessageBody
case class ImageBody(url: String) extends MessageBody
case class TextBody(text: String) extends MessageBody
case class Participant(email: String)

case class Message(participant: Participant, body: Seq[MessageBody])

type Parser[A] = A => String

def parse[A](a: A)(implicit parser: Parser[A]): String = {
  parser(a)
}

implicit val imageBodyParser: Parser[ImageBody] = "\"Image\":" + _.url
implicit val textBodyParser: Parser[TextBody] = "\"Text\":" + _.text
implicit def messageBodyParser(item: MessageBody) = {
  item match {
    case imageBody: ImageBody => parse(imageBody)
    case textBody: TextBody => parse(textBody)
  }
}
implicit val participantParser: Parser[Participant] = "\"Email\":" + _.email
implicit def seqParser[A](seq:Seq[A])(implicit f:Parser[A]) = "[" + seq.map(x => parse(x)).mkString(",") + "]"

implicit def messageParser(message: Message) = {
  "\"Participant\":" + parse(message.participant) + "\n" +
  "\"Body\":" + parse(message.body)
}

parse(Message(Participant("amitmarx@gmail.com"), Seq(TextBody("Hey"), ImageBody("url"))))
