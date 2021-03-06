package org.ladderframework.html.form

import scala.xml.NodeSeq
import org.ladderframework.js.JsCmd
import org.ladderframework.Context
import org.ladderframework.Utils
import org.ladderframework.HttpResponse
import scala.concurrent.Future
import org.ladderframework.Status
import org.ladderframework.NotImplemented
import org.ladderframework.InternalServerError
import scala.concurrent.ExecutionContext

case class FormContext(data: Map[String, String], indexesOf: String => Seq[Int], errors: Seq[FormError])

trait FormRendering {
	def transform(ns: NodeSeq): NodeSeq
	type FormId = String
	
}

case class Ajax[M <: Mapping[M]](form: Form[M])
		(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[JsCmd])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context) extends FormRendering{
	val id = Utils.uuid
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
	val id = Utils.uuid
	val actionPath = context.addSubmitCallback(req => {
		val boundForm = form.bindFromRequest(req.parameters.mapValues(_.toSeq))
		val either = if(boundForm.hasErrors) Left[Form[M], M#T](boundForm) else boundForm.value.toRight(boundForm)
		callback(either, id).recover{
			case t:NotImplementedError => List("error", NotImplemented.code.toString) -> context.boot.error(NotImplemented, Some(t))
			case t => List("error", InternalServerError.code.toString) -> context.boot.error(InternalServerError, Some(t))
		}
	})
	
	def transform(ns: NodeSeq): NodeSeq = <form id={id} method={method} action={actionPath.mkString("/", "/", "")}>{rendering(form.context)(form.mapping)(ns)}</form>
}

case class StatefulPost[M <: Mapping[M]](form: Form[M])
		(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
		(implicit context: Context, executionContext: ExecutionContext) extends StatefulForm[M]("POST", form, callback, rendering)
		
case class StatefulGet[M <: Mapping[M]](form: Form[M])
	(callback: (Either[Form[M], M#T], FormRendering#FormId) => Future[(List[String], HttpResponse)])(rendering: FormContext => M => (NodeSeq => NodeSeq))
	(implicit context: Context, executionContext: ExecutionContext) extends StatefulForm[M]("GET", form, callback, rendering)




