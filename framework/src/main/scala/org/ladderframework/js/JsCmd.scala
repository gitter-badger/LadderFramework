
package org.ladderframework{

	import scala.xml.NodeSeq
	import org.ladderframework.json._
	
	package object js{
		implicit def string2JsArg(string:String):JsStringArg = JsStringArg(string)
		implicit def nodeSeq2JsNodeSeqArg(ns:NodeSeq):JsNodeSeqArg = JsNodeSeqArg(ns)
		
		
		implicit class JSInterpolation(val sc: StringContext) extends AnyVal {
			def js(args: JValue*): JsCmd = {
				val strings = sc.parts.iterator
				val expressions = args.iterator
				val buf = new StringBuffer(strings.next)
				while(strings.hasNext) {
					buf append expressions.next.nospace
					buf append strings.next
				}
				JsRaw(buf.toString)
			}
		}
		
		private case class JsRaw(cmd: String) extends JsCmd{
			def toCmd = cmd
		} 
		
	}
	
	package js{
		trait JsCmd{
			def toCmd:String
		}
		
		case class JsCall(method:String, args:JsArg*) extends JsCmd{
			override def toCmd = method + "(" + args.map(_.arg).mkString(",") + ");"
		}
		
		case object JsNoCmd extends JsCmd{
			override def toCmd = ""
		}
		
		trait JsArg{
			def arg:String
		}
		
		case class JsonArg(jValue: JValue) extends JsArg{
			def arg = jValue.nospace
		}
		
		case class JsStringArg(string:String) extends JsArg{
			def arg = "\"" + string.replaceAll("\"", "\\\"") + "\""
		}
		
		case class JsNodeSeqArg(ns:NodeSeq) extends JsArg{
			def arg = "\"" + ns.mkString.replaceAll("[\"]","""\\"""").replaceAll("\\s+", " ") + "\""
		}
		
		case class JsSetHtml(selector: String, ns: NodeSeq) extends JsCmd{
			def toCmd = "$(\"" + selector + "\").html(\"" + ns.mkString.replaceAll("[\"]","""\\"""").replaceAll("\\s+", " ") + "\");"
		}
	}
}