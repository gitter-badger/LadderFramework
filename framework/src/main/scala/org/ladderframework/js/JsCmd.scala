package org.ladderframework{

	import scala.xml.NodeSeq
	
	package object js{
		implicit def string2JsArg(string:String):JsStringArg = JsStringArg(string)
		implicit def nodeSeq2JsNodeSeqArg(ns:NodeSeq):JsNodeSeqArg = JsNodeSeqArg(ns)
		
		
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