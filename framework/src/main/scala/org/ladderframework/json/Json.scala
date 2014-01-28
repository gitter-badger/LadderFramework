package org.ladderframework

import scala.util.Try

package json{

	sealed trait JValue
	
	case class JString(value: String) extends JValue 
	
	case class JDouble(value: Double) extends JValue
	
	case class JInt(value: Int) extends JValue
	
	case class JBoolean(value: Boolean) extends JValue
	
	case class JObject(value: Map[String, JValue]) extends JValue
	object JObject{
		def apply(values: (String, JValue)*): JObject = JObject(values.toMap)
	}
	
	case class JArray(values: JValue*) extends JValue
	
	case object JNull extends JValue
	
	case class UnParasableException(msg: String, cause: Throwable) extends Exception(msg, cause)

}

package object json{
	
	implicit class JsonRenderer(value: JValue){
		def nospace: String = value match {
			case JString(s) => s""""$s""""
			case JBoolean(b) => b.toString
			case JDouble(d) => d.toString
			case JInt(i) => i.toString
			case JNull => "null"
			case array:JArray => array.values.map(_.nospace).mkString("[", ",", "]")
			case JObject(m) => "{" + m.mapValues(_.nospace).map(i => "\"" + i._1 + "\"" + ":" + i._2).mkString(",") + "}"
			case null => "null"
 		}
		def pretty: String = pretty(0, "\n")
		protected def pretty(indent: Int = 0, endLine: String = "\n"): String = {
			val inStr = "  " * indent
			value match {
				case JString(s) => inStr + s""""$s"""" + endLine
				case JBoolean(b) => inStr +  b.toString + endLine
				case JDouble(d) => inStr + d.toString + endLine
				case JInt(i) => inStr + i.toString + endLine
				case JNull => inStr + "null" + endLine
				case array:JArray => array.values.map(_.pretty(indent + 1, "")).mkString("[\n", ",\n" , "\n" + inStr + "]" + endLine)
				case JObject(m) => inStr + "{\n" + m.mapValues(_.pretty(indent + 1, "").trim).map(i => inStr + "  \"" + i._1 + "\"" + ":" + i._2).mkString(",\n") + "\n" + inStr + "}" + endLine
				case null => inStr + "null" + endLine
			}
		}
	}
	
	implicit def string2JString(str: String): JString = new JString(str)
	implicit def int2JInt(int: Int): JInt = new JInt(int)
	implicit def int2JDouble(double: Double): JDouble = new JDouble(double)
	implicit def boolean2JBoolean(boolean: Boolean): JBoolean = JBoolean(boolean)
	
	import scala.util.parsing.combinator._

	object JsonParser extends JavaTokenParsers {   
	
	  def value: Parser[JValue] = obj | array | 
	                            string | (int ||| double) | nullParser | boolean

	  def string: Parser[JString] = stringLiteral ^^ (JString(_))                          
	  def int: Parser[JInt] = wholeNumber ^^ (n => JInt(n.toInt))                          
	  def double: Parser[JDouble] = """-?\d*\.\d+""".r ^^ (n => JDouble(n.toDouble))                          
	                            
    def nullParser: Parser[JValue] = "null" ^^ (_ => JNull)
	                            
    def boolean: Parser[JBoolean] = ("true" | "false") ^^ (m => JBoolean(m.toBoolean))
	
	  def obj: Parser[JObject] = "{"~>repsep(member, ",")<~"}" ^^ (o => JObject(o.toMap))
	
	  def array: Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ (a => JArray(a:_*))
	
	  override def stringLiteral: Parser[String] =
    	"\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""
	  
	  def member: Parser[(String, JValue)] = stringLiteral~":"~value ^^ (m => m._1._1 -> m._2)
	}

	implicit class JsonString(input: String){
		def toJson: Try[JValue] = {
			val r = JsonParser.parseAll(JsonParser.value, input)
			Try{
				r.get
			}.recover{case t => throw new UnParasableException(r.toString, t)}
		}
	}
	
}

