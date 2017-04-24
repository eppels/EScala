package week3

import java.util.Date

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  def stringify = values.map {case (name, value) =>
    "\"" + name + "\":" + value.stringify
  }.mkString("{", ", ", "}")
}


final case class JsString(value: String) extends JsValue {
  def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
}

trait JsonWriter[A] {
  def write(data: A) : JsValue
}

object Utils {
  implicit class ToJson[A](data: A) {
    def toJson(implicit writer: JsonWriter[A]) = writer.write(data)
  }
}

sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime() - createdAt.getTime()
}
final case class Anonymous(
                            val id: String,
                            val createdAt: Date = new Date()
                            ) extends Visitor
final case class User(
                       val id: String,
                       val email: String,
                       val createdAt: Date = new Date()
                       ) extends Visitor

object VisitorWriters {
  import Utils._
  implicit object AnonymousWriter extends JsonWriter[Anonymous] {
    override def write(data: Anonymous): JsValue = JsObject(Map(
      "id" -> JsString(data.id),
      "createdAt" -> JsString(data.createdAt.toString)
    ))
  }
  implicit object UserWriter extends JsonWriter[User] {
    override def write(data: User): JsValue = JsObject(Map(
      "id" -> JsString(data.id),
      "email" -> JsString(data.email),
      "createdAt" -> JsString(data.createdAt.toString)
    ))
  }
  implicit object VisitorWriter extends JsonWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User => user.toJson
    }
  }
}

object TestDrive extends App {
  import VisitorWriters._
  import Utils._
  val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))
  val visitorsJson = visitors.map(_.toJson)
  visitorsJson.foreach(s => println(s.stringify))
}