package org.ladderframework.html

import scala.xml.NodeSeq
import org.ladderframework.js.JsCmd
import org.ladderframework.Context
import org.ladderframework.utils
import org.ladderframework.HttpResponse
import scala.concurrent.Future
import org.ladderframework.Status
import scala.concurrent.ExecutionContext
import org.ladderframework.form._

trait FormRendering {
	def transform(ns: NodeSeq): NodeSeq
	type FormId = String
	
}

case class Ajax[M <: Mapping[M]](form: Form[M])
		(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[JsCmd])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context) extends FormRendering{
	val id = utils.uuid
	val submitPath = context.addAjaxFormSubmitCallback(req => {
		val boundForm = form.bindFromRequest(req.parameters.mapValues(_.toSeq))
		val either = if(boundForm.hasErrors) Left[Form[M], M#T](boundForm) else boundForm.value.toRight(boundForm)
		callback(either, id)
	})
	
	def transform(ns: NodeSeq): NodeSeq = <form id={id} onsubmit="return ladder.post(event);" action={submitPath}>{
			rendering(form.context)(form.mapping)(ns)
		}</form>
}

abstract class StatefulForm[M <: Mapping[M]](
			method: String, 
			form: Form[M], 
			callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[(List[String], HttpResponse)], 
			rendering: FormContext => M => (NodeSeq => NodeSeq)
		)(implicit context: Context, executionContext: ExecutionContext) extends FormRendering{
	val id = utils.uuid
	val actionPath = context.addSubmitCallback(req => {
		val boundForm = form.bindFromRequest(req.parameters.mapValues(_.toSeq))
		val either = if(boundForm.hasErrors) Left[Form[M], M#T](boundForm) else boundForm.value.toRight(boundForm)
		callback(either, id).recover{
			case t:NotImplementedError => List("error", Status.NotImplemented.code.toString) -> context.boot.error(Status.NotImplemented, Some(t))
			case t => List("error", Status.InternalServerError.code.toString) -> context.boot.error(Status.InternalServerError, Some(t))
		}
	})
	
	def transform(ns: NodeSeq): NodeSeq = <form id={id} 
		accept-charset="UTF-8" 
		method={method} 
		action={actionPath.mkString("/", "/", "")}>{
			rendering(form.context)(form.mapping)(ns)
		}</form>
}

case class StatefulPost[M <: Mapping[M]](form: Form[M])
		(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context, executionContext: ExecutionContext) extends StatefulForm[M]("POST", form, callback, rendering)
		
case class StatefulGet[M <: Mapping[M]](form: Form[M])
	(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
	(implicit context: Context, executionContext: ExecutionContext) extends StatefulForm[M]("GET", form, callback, rendering)




