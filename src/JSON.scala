/**
 * Extended Exercise 2 - JSON
 * I chose to do this one using polymorphism because I wanted each concrete class to have a good toString implementation
 */
sealed trait JSONData
final case class JSONNumber(value : Double) extends JSONData {
  override def toString = value.toString
}
final case class JSONString(value : String) extends JSONData {
  override def toString = "\"" + value + "\""
}
final case class JSONBoolean(value : Boolean) extends JSONData {
  override def toString = value.toString
}
final case class JSONArray(elements: Seq[JSONData]) extends JSONData {
  override def toString = "[" + elements.map(_.toString).mkString(", ")+ "]"
}
final case class JSONObject(elements: Seq[(String, JSONData)]) extends JSONData {
  override def toString = "{" + elements.map {case (key, value) => "\"" + key + "\": " + value.toString}.mkString(", ") + "}"
}

object SerializeJSON {
  def apply(jsonElements: Seq[JSONData]) : String = {
    jsonElements.map(_.toString).mkString(", \n")
  }
}

object JSONTest extends App {
  val el1 = JSONArray(Seq(JSONString("a string"), JSONNumber(1.0), JSONBoolean(true)))
  val el2 = JSONObject(Seq(("a", JSONArray(Seq(JSONNumber(1), JSONNumber(2), JSONNumber(3)))),
    ("b", JSONArray(Seq(JSONString("a"), JSONString("b"), JSONString("c")))),
    ("c", JSONObject(Seq(("doh", JSONBoolean(true)), ("ray", JSONBoolean(false)), ("me", JSONNumber(1)))))
  ))
  val allElements = Seq(el1, el2)
  val resultText = SerializeJSON(allElements)
  println(resultText)
}
