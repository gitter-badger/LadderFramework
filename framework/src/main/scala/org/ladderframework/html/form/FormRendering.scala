package org.ladderframework.html.form

import scala.xml.NodeSeq
import org.ladderframework.js.JsCmd
import org.ladderframework.Context
import org.ladderframework.Utils
import org.ladderframework.HttpResponse
import scala.concurrent.Future

case class FormContext(data: String => Option[String], errors: Seq[FormError])

trait FormRendering {
	def transform(ns: NodeSeq): NodeSeq
	type FormId = String
	
}

case class Ajax[M <: Mapping](form: Form[M])
		(callback: (Either[Form[M], Option[M#T]], FormRendering#FormId) => Future[JsCmd])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context) extends FormRendering{
	val id = Utils.uuid
	context.addAjaxFormSubmitCallback(req => {
		val boundForm = form.bindFromRequest(req.parameters.mapValues(_.toSeq))
		val either = if(boundForm.hasErrors) Left[Form[M], Option[M#T]](boundForm) else Right[Form[M], Option[M#T]](boundForm.value)
		callback(either, id)
	})
	
	def transform(ns: NodeSeq): NodeSeq = <form id={id}>{rendering(form.context)(form.mapping)(ns)}</form>
}

abstract class StatefullForm[M <: Mapping](
			method: String, 
			form: Form[M], 
			callback: (Either[Form[M], Option[M#T]], FormRendering#FormId) => Future[(List[String], HttpResponse)], 
			rendering: FormContext => M => (NodeSeq => NodeSeq)
		)(implicit context: Context) extends FormRendering{
	val id = Utils.uuid
	val actionPath = context.addSubmitCallback(req => {
		val boundForm = form.bindFromRequest(req.parameters.mapValues(_.toSeq))
		val either = if(boundForm.hasErrors) Left[Form[M], Option[M#T]](boundForm) else Right[Form[M], Option[M#T]](boundForm.value)
		callback(either, id)
	})
	
	def transform(ns: NodeSeq): NodeSeq = <form id={id} method={method}>{rendering(form.context)(form.mapping)(ns)}</form>
}

case class StatefullPost[M <: Mapping](form: Form[M])
		(callback: (Either[Form[M], Option[M#T]], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context) extends StatefullForm[M]("POST", form, callback, rendering)
		
case class StatefullGet[M <: Mapping](form: Form[M])
	(callback: (Either[Form[M], Option[M#T]], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
	(implicit context: Context) extends StatefullForm[M]("GET", form, callback, rendering)




