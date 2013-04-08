package org.ladderframework.css

import scala.xml._
import scala.xml.transform._
import MetaData._
import org.ladderframework.html.form.FormRendering
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait CssSelector {
	type NSTransform = NodeSeq => NodeSeq
	def #> (ns:NodeSeq):NodeSeq => NodeSeq
	def #> (nsf:NodeSeq => NodeSeq):NodeSeq => NodeSeq
	def #> (e: FormRendering):NodeSeq => NodeSeq
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq
	def #@> (attrs:Map[String, String]):NodeSeq => NodeSeq
	def matches(n:Node):Boolean
	
	class CssTransformer(rr: PartialFunction[Node, Seq[Node]]) extends BasicTransformer{
		override def transform(n: Node): Seq[Node] = {
			val ns = super.transform(n)
			ns.flatMap(rr.orElse{case (n:Node) => Seq(n)})
		}
		
		override def transform(ns: Seq[Node]): Seq[Node] = {
	    if (ns.isEmpty) ns
	    else transform(ns.head) ++ transform(ns.tail)
	  }
	}
	
	private[css] def transform(ns: NodeSeq, rr: PartialFunction[Node, Seq[Node]]): NodeSeq = new CssTransformer(rr).transform(ns)
}

object CssSelector {
	
	implicit def stringToCssSelector(css:String): CssSelector = {
		css match {
			case IdSelector(id) => new IdSelector(id)
			case ClassSelector(id) => new ClassSelector(id)
			case ElementSelector(id) => new ElementSelector(id)
			case AttributeSelector(key, value) => new AttributeSelector(key, value)
			case ElementAttributeSelector(tagName, key, value) => new ElementAttributeSelector(tagName, key, value)
			case any @ _ => new PassThruSelector(any)
		}
	}
	
	def mapToAttribute(map:Map[String, String]): MetaData = {
		map.toSeq.map{case (k, v) => Attribute(null, k, v, null)}.foldLeft[MetaData](Null)((as, a) => a.copy(as))
	}
	
	implicit def stringToFindNode(css:String):Node => Boolean = {
		stringToCssSelector(css).matches
	}
	
	implicit def pimpNodeSeq(ns:NodeSeq) = new NodeSeqPimper(ns)
	
	implicit def pimpNodeSeqFunc(nsf:NodeSeq => NodeSeq) = new NodeSeqFuncPimper(nsf)	
	
	implicit def stringToTextNode(text:String):Node = Option(text).map(Text(_)).getOrElse(Text(""))
}

private[ladderframework] class PassThruSelector(val any:String) extends CssSelector{
	def #> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #> (nsf:NodeSeq => NodeSeq):NodeSeq => NodeSeq = ns=>ns
	def #> (e:FormRendering):NodeSeq => NodeSeq = ns => ns
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = ns => ns
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #@> (attrs: Map[String, String]): NodeSeq => NodeSeq = ns => ns
	def matches(n:Node):Boolean = false
}

private[ladderframework] trait AttribSelector extends CssSelector{
	
	def attributeMatches(attribs:MetaData):Boolean
	
	def #> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr: PartialFunction[Node, Seq[Node]] = {case Elem(_, _, attribs, _, _*) if attributeMatches(attribs) => ns}
		
		ns => transform(ns, rr)
	}
	
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {case Elem(p, l, attribs, s, _*) if attributeMatches(attribs) => Elem(p,l,attribs, s, false, ns:_*)}
		nodeSeq => transform(nodeSeq, rr)
	}
	
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = { 
			case Elem(p, l, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	Elem(p,l,attribs, s, false, (children ++ ns):_*)
		}
		nodeSeq => transform(nodeSeq, rr)
	}
	
	def #> (nsTransform:NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] =  {
				case e @ Elem(p, l, attribs, s, children @ _*) if attributeMatches(attribs) =>
					val transformedChildren = nsTransform(children)
					Elem(p,l,attribs, s, false, transformedChildren:_*)
		}
		ns => transform(ns, rr)
	}
	def #> (e: FormRendering):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
				case el @ Elem(_, _, attribs, _, _ *) if attributeMatches(attribs) => 
					e.transform(el)
		}
		ns => transform(ns, rr)
	}
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
			val rr:PartialFunction[Node, Seq[Node]] = {
						case e if attributeMatches(e.attributes) =>
							iter.flatMap(_.apply(e)).toSeq							
			}
			ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
						case e @ Elem(p, l, attribs, n, children @ _*) if attributeMatches(attribs) =>
							val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
							Elem(p,l, concatenatedAttrs, n, false, children:_*)
			}
			ns => transform(ns, rr)
	}
	
	def matches(n:Node):Boolean = {
		n match {
			case Elem(_, _, attribs, _, _*) if attributeMatches(attribs) => true
			case _ => false
		}
	}
}

private[ladderframework] class IdSelector(val id:String) extends AttribSelector{
	def attributeMatches(attribs:MetaData) = attribs.asAttrMap.get("id").map(_ == id).getOrElse(false) 
	
	override def toString = "IdSelector(id: " + id + ")"
}
object IdSelector{
	def unapply(selector:String):Option[String] = {
		if(selector.headOption.map(_ == '#').getOrElse(false)) Some(selector.tail) else None
	} 
}

private[ladderframework] class ClassSelector(val clazz:String) extends AttribSelector{
	def attributeMatches(attribs:MetaData) = attribs.asAttrMap.get("class").
			map(attr => (" " + attr + " ").contains(" " + clazz + " ")).getOrElse(false) 
			
	override def toString = "ClassSelector(class: " + clazz + ")"
}
object ClassSelector{
	def unapply(selector:String):Option[String] = {
			if(selector.headOption.map(_ == '.').getOrElse(false)) Some(selector.tail) else None
	} 
}
private[ladderframework] class ElementSelector(val element:String) extends CssSelector{
	
	def #> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr: PartialFunction[Node, Seq[Node]] = {
			 case Elem(_, `element`, _, _, _*) => ns
		}
		ns => transform(ns, rr)
	}
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = {
			val rr: PartialFunction[Node, Seq[Node]] = {
				case e @ Elem(p, `element`, attribs, s, _*) =>
								Elem(p, element, attribs, s, false, ns:_*)
			}
			ns => transform(ns, rr)
	}
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = {
			val rr:PartialFunction[Node, Seq[Node]] = {
				case e @ Elem(p, `element`, attribs, s, children @ _*) =>
					val newChilren = (children ++ ns)
					Elem(p, element, attribs, s, false, newChilren:_*)
			}
			ns => transform(ns, rr)
	}
	
	def #> (nsTransform:NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			case Elem(_, `element`, _, _, children @ _*) => 
				nsTransform(children)
		}
		ns => transform(ns, rr)
	}
	def #> (e: FormRendering):NodeSeq => NodeSeq = {
			val rr:PartialFunction[Node, Seq[Node]] = {
				case el @ Elem(_, `element`, _, _, children @ _*) => 
					e.transform(el)
			}
			ns => transform(ns, rr)
	}
	
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			    case e @ Elem(p, `element`, a, n, children @ _*) =>
			    	iter.flatMap(_.apply(e)).toSeq
		}
		ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
					case Elem(p, `element`, attribs, n, children @ _*) =>
						val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
						Elem(p, element, concatenatedAttrs, n, false, children:_*)
		}
		ns => transform(ns, rr)
	}
	
	def matches(n:Node):Boolean = {
		n match {
			case Elem(_, `element`, _, _, _*) => true
			case _ => false
		}
	}
	
	override def toString = "ElementSelector(element: " + element + ")"
}
object ElementSelector{
	def unapply(selector:String):Option[String] = {
			if(selector.matches("[\\d\\w_-]+")) Some(selector) else None
	} 
}

private[ladderframework] class AttributeSelector(attrib:String, value:String) extends AttribSelector{
	def attributeMatches(attribs:MetaData) = attribs.asAttrMap.get(attrib).
			map(attr => (" " + attr + " ") == (" " + value + " ")).getOrElse(false) 
			
	override def toString = "AttributeSelector(" + attrib + ": " + value + ")"
}

object AttributeSelector{
	val RegExp = "\\[([\\d\\w_-]+)=([\\d\\w_-]+)\\]".r
	def unapply(selector:String):Option[(String, String)] = {
		selector match {
			case RegExp(key, value) => Some(key, value)
			case _ => None
		}
	}
}

private[ladderframework] class ElementAttributeSelector(tagName:String, attrib:String, value:String) extends CssSelector{
	
	def attributeMatches(attribs:MetaData) = attribs.asAttrMap.get(attrib).
			map(attr => (" " + attr + " ") == (" " + value + " ")).getOrElse(false) 
			
	def #>(ns: NodeSeq): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			    case Elem(_, `tagName`, attribs, _, _*) if attributeMatches(attribs) => ns
		}
		ns => transform(ns, rr)
	}
	
	def #>>(ns: NodeSeq): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			    case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	Elem(p, tagName, attribs, s, false, ns:_*)
		}
		ns => transform(ns, rr)
	}
	
	def #+>(ns: NodeSeq): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			    case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	val newChildren = (children ++ ns)
			    	Elem(p, tagName, attribs, s, false, newChildren:_*)
		}
		ns => transform(ns, rr)
	}
	
	def #>(nsTransform: NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
			case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
				val transformedChildren = nsTransform(children)
				Elem(p, tagName, attribs, s, false, transformedChildren:_*)
		}
		ns => transform(ns, rr)
	}
	
	def #>(e: FormRendering): NodeSeq => NodeSeq = {
			val rr:PartialFunction[Node, Seq[Node]] = {
				case el @ Elem(_, `tagName`, attribs, _, children @ _*) if attributeMatches(attribs) => 
					e.transform(el)
			}
			ns => transform(ns, rr)
	}
	def #>(iter: Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
			val rr:PartialFunction[Node, Seq[Node]] = {
						case e @ Elem(p, `tagName`, attribs, n, children @ _*) if attributeMatches(attribs) => 
							iter.flatMap(_.apply(e)).toSeq
			}
			ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr:PartialFunction[Node, Seq[Node]] = {
					case Elem(p, `tagName`, attribs, n, children @ _*) if attributeMatches(attribs) => 
						val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
						Elem(p, tagName, concatenatedAttrs, n, false, children:_*)
		}
		ns => transform(ns, rr)
	}
	
	def matches(n:Node):Boolean = {
		n match {
			case Elem(_, `tagName`, attribs, _, _*) if attributeMatches(attribs) => true
			case _ => false
		}
	}
	
	override def toString = "ElementAttributeSelector(" + tagName + "::" + attrib + ": " + value + ")"
	
}

object ElementAttributeSelector{
	val RegExp = "([\\d\\w]+)\\[([\\d\\w_-]+)=([\\d\\w_-]+)\\]".r
			def unapply(selector:String):Option[(String, String, String)] = {
			selector match {
			case RegExp(tagName, key, value) => Some(tagName, key, value)
			case _ => None
			}
	}
}

class NodeSeqPimper(ns:NodeSeq){
	def extract(cssSelector:CssSelector):Seq[Node] = {
		def find(searchIn:Seq[Node]):Seq[Node] = {
			searchIn.flatMap(n => {
				if(cssSelector.matches(n)){
					n ++ find(n.child)
				}else{
					find(n.child)
				} 
			})
		}
		find(ns)
	}
}
class NodeSeqFuncPimper(nsf:NodeSeq => NodeSeq){
	def &(other:NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		nsf.andThen(other)
	}
}

