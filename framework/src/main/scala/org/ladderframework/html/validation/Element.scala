package org.ladderframework.html.validation

import org.ladderframework.html.form.InputValueElement
import org.ladderframework.js.JsCmd
import org.ladderframework.Context
import org.ladderframework.html.form.Element

case class ValidationError(msg: String)

case class ValidationMsg(forInput: InputValueElement[_])(implicit val context: Context) extends Element {
	override def appendAttributes = Map("class" -> forInput.validationId)
	override val tagName = "span"
}

object ValidationMsg {
	implicit def ive2wapper(ive: InputValueElement[_]): InputValueElementWrapper = new InputValueElementWrapper(ive)
}

class InputValueElementWrapper(ive: InputValueElement[_]) {
	def msg(implicit context: Context): ValidationMsg = ValidationMsg(ive)
}

case class MultiJsCmd(cmds: JsCmd*) extends JsCmd {
	override val toCmd: String = cmds.map(_.toCmd).mkString("\n")

	def &(cmd: JsCmd) = MultiJsCmd((cmds :+ cmd): _*)
}
case class JsClearValidationErrors(elements: InputValueElement[_]*) extends JsCmd {
	def toCmd: String = {
		elements.map(element => {
			"$('." + element.validationId + "').html('');"
		}).mkString("")
	}
	def &(cmd: JsCmd) = MultiJsCmd(this, cmd)
}
case class JsValidationError(error: (String, ValidationError)) extends JsCmd {
	def toCmd: String = {
		"$('." + error._1 + "').text('" + error._2.msg + "')"; 
	}
	def &(cmd: JsCmd) = MultiJsCmd(this, cmd)
}