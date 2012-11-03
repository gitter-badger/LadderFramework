package org.ladderframework.css

import scala.xml._
import scala.xml.transform._
import MetaData._
import org.ladderframework.html.form.Element

trait CssSelector {
	def #> (ns:NodeSeq):NodeSeq => NodeSeq
	def #> (nsf:NodeSeq => NodeSeq):NodeSeq => NodeSeq
	def #> (e:Element):NodeSeq => NodeSeq
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq
	def #@> (attrs:Map[String, String]):NodeSeq => NodeSeq
	def matches(n:Node):Boolean
	
	private[css] def transform(ns: NodeSeq, rr: RewriteRule) = new RuleTransformer(rr).transform(ns)
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
	
	implicit def stringToTextNode(text:String):Node = Text(text)
}

private[ladderframework] class PassThruSelector(val any:String) extends CssSelector{
	def #> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #> (nsf:NodeSeq => NodeSeq):NodeSeq => NodeSeq = ns=>ns
	def #> (e:Element):NodeSeq => NodeSeq = ns => ns
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = ns => ns
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = ns => ns
	def #@> (attrs: Map[String, String]): NodeSeq => NodeSeq = ns => ns
	def matches(n:Node):Boolean = false
}

private[ladderframework] trait AttribSelector extends CssSelector{
	
	def attributeMatches(attribs:MetaData):Boolean
	
	def #> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case Elem(_, _, attribs, _, _*) if attributeMatches(attribs) => ns
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case e @ Elem(p, l, attribs, s, _*) if attributeMatches(attribs) =>
			    	Elem(p,l,attribs, s, ns.isEmpty, ns:_*)
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case e @ Elem(p, l, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	Elem(p,l,attribs, s, (children ++ ns).isEmpty, (children ++ ns):_*)
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #> (nsTransform:NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = n match {
			case e @ Elem(p, l, attribs, s, children @ _*) if attributeMatches(attribs) =>
				val transformedChildren = nsTransform(children)
				Elem(p,l,attribs, s, transformedChildren.isEmpty, transformedChildren:_*)
			case other => other
			}
		}
		ns => transform(ns, rr)
	}
	def #> (e:Element):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = n match {
				case el @ Elem(_, _, attribs, _, children @ _*) if attributeMatches(attribs) => 
					e.transform(el)
				case other => other
				}
			}
			ns => transform(ns, rr)
	}
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = {
						n match {
						case Elem(p, l, attribs, n, children @ _*) if attributeMatches(attribs) =>
							val trChldrn = iter.flatMap(_.apply(children)).toSeq
							Elem(p,l,attribs, n, trChldrn.isEmpty, trChldrn:_*)
						case other => other
						}
				}
			}
			ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = {
						n match {
						case Elem(p, l, attribs, n, children @ _*) if attributeMatches(attribs) =>
							val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
							Elem(p,l, concatenatedAttrs, n, children.isEmpty, children:_*)
						case other => other
						}
				}
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
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case Elem(_, `element`, _, _, _*) => ns
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	def #>> (ns:NodeSeq):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = {
						n match {
							case e @ Elem(p, `element`, attribs, s, _*) =>
								Elem(p, element, attribs, s, ns.isEmpty, ns:_*)
							case other => other
						}
				}
			}
			ns => transform(ns, rr)
	}
	def #+> (ns:NodeSeq):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = {
						n match {
							case e @ Elem(p, `element`, attribs, s, children @ _*) =>
								val newChilren = (children ++ ns)
								Elem(p, element, attribs, s, newChilren.isEmpty, newChilren:_*)
							case other => other
						}
				}
			}
			ns => transform(ns, rr)
	}
	
	def #> (nsTransform:NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = n match {
			case Elem(_, `element`, _, _, children @ _*) => 
				nsTransform(children)
			case other => other
			}
		}
		ns => transform(ns, rr)
	}
	def #> (e:Element):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = n match {
				case el @ Elem(_, `element`, _, _, children @ _*) => 
					e.transform(el)
				case other => other
				}
			}
			ns => transform(ns, rr)
	}
	
	def #> (iter:Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case Elem(p, `element`, a, n, children @ _*) =>
			    	val trChildren = iter.flatMap(_.apply(children)).toSeq
			    	Elem(p, element, a, n, trChildren.isEmpty, trChildren:_*)
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
					n match {
					case Elem(p, `element`, attribs, n, children @ _*) =>
						val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
						Elem(p, element, concatenatedAttrs, n, children.isEmpty, children:_*)
					case other => other
					}
			}
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
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case Elem(_, `tagName`, attribs, _, _*) if attributeMatches(attribs) => ns
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #>>(ns: NodeSeq): NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	Elem(p, tagName, attribs, s, ns.isEmpty, ns:_*)
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #+>(ns: NodeSeq): NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
			  n match {
			    case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
			    	val newChildren = (children ++ ns)
			    	Elem(p, tagName, attribs, s, newChildren.isEmpty, newChildren:_*)
			    case other => other
			  }
			}
		}
		ns => transform(ns, rr)
	}
	
	def #>(nsTransform: NodeSeq => NodeSeq):NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = n match {
			case e @ Elem(p, `tagName`, attribs, s, children @ _*) if attributeMatches(attribs) =>
				val transformedChildren = nsTransform(children)
				Elem(p, tagName, attribs, s, transformedChildren.isEmpty, transformedChildren:_*)
			case other => other
			}
		}
		ns => transform(ns, rr)
	}
	
	def #>(e: Element): NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = n match {
				case el @ Elem(_, `tagName`, attribs, _, children @ _*) if attributeMatches(attribs) => 
					e.transform(el)
				case other => other
				}
			}
			ns => transform(ns, rr)
	}
	def #>(iter: Iterable[NodeSeq => NodeSeq]):NodeSeq => NodeSeq = {
			val rr = new RewriteRule {
				override def transform(n: Node): Seq[Node] = {
						n match {
						case Elem(p, `tagName`, attribs, n, children @ _*) if attributeMatches(attribs) => 
							val transformedChildren = iter.flatMap(_.apply(children)).toSeq
							Elem(p, tagName, attribs, n, transformedChildren.isEmpty, transformedChildren:_*)
						case other => other
						}
				}
			}
			ns => transform(ns, rr)
	}
	
	def #@> (newAttrs: Map[String, String]): NodeSeq => NodeSeq = {
		val rr = new RewriteRule {
			override def transform(n: Node): Seq[Node] = {
				n match {
					case Elem(p, `tagName`, attribs, n, children @ _*) if attributeMatches(attribs) => 
						val concatenatedAttrs = concatenate(CssSelector.mapToAttribute(newAttrs), attribs)
						Elem(p, tagName, concatenatedAttrs, n, children.isEmpty, children:_*)
					case other => other
				}
			}
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

