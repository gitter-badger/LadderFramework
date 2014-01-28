package org.ladderframework.html.form

import scala.annotation.implicitNotFound
import org.ladderframework.Utils
import org.ladderframework.json.JValue

/**
 * Helper to manage HTML form description, submission and validation.
 *
 * For example, a form handling a `User` case class submission:
 * {{{
 * import play.api.data._
 * import play.api.data.format.Formats._
 *
 * val userForm = Form(
 *   mapping(
 *     of[String],
 *     of[Int],
 *     of[String]
 *   )(User.apply)(User.unapply)
 * )
 * }}}
 *
 * @tparam T the type managed by this form
 * @param mapping the form mapping, which describes all form fields
 * @param data the current form data, used to display the form
 * @param errors the collection of errors associated with this form
 * @param value a concrete value of type `T` if the form submission was successful
 */
case class Form[M <: Mapping[M]](mapping: M, data: Map[String, String], errors: Seq[FormError], value: Option[M#T]) {
	
	/* *
	 * Constraints associated with this form, indexed by field name.
	 */
	/*val constraints: Map[String, Seq[(String, Seq[Any])]] = mapping.mappings.map { m =>
    m.key -> m.constraints.collect { case Constraint(Some(name), args) => name -> args }
  }.filterNot(_._2.isEmpty).toMap
	 */
	/* *
	 * Formats associated to this form, indexed by field name. *
	 */
	/*
  val formats: Map[String, (String, Seq[Any])] = mapping.mappings.map { m =>
    m.key -> m.format
  }.collect {
    case (k, Some(f)) => k -> f
  }.toMap
	 */
	/**
	 * Binds data to this form, i.e. handles form submission.
	 *
	 * @param data the data to submit
	 * @return a copy of this form, filled with the new data
	 */
	def bind(data: Map[String, String]): Form[M] = mapping.bind(data).fold(
		errors => this.copy(data = data, errors = errors, value = None),
		value => this.copy(data = data, errors = Nil, value = Some(value)))

	  /**
	   * Binds data to this form, i.e. handles form submission.
	   *
	   * @param data Json data to submit
	   * @return a copy of this form, filled with the new data
	   */
	  def bind(data: JValue): Form[M] = {
		 	import org.ladderframework.json._
		  def fromJson(prefix: String = "", js: JValue): Map[String, String] = js match {
		    case JObject(fields) => {
		      fields.map { case (key, value) => 
		      	fromJson(Option(prefix).filterNot(_.isEmpty).map(_ + ".").getOrElse("") + key, value) 
		      }.foldLeft(Map.empty[String, String])(_ ++ _)
		    }
		    case arr:JArray => {
		      arr.values.zipWithIndex.map { case (value, i) => fromJson(prefix + "[" + i + "]", value) }.foldLeft(Map.empty[String, String])(_ ++ _)
		    }
		    case JNull => Map.empty
		    case JBoolean(value) => Map(prefix -> value.toString)
		    case JInt(value) => Map(prefix -> value.toString)
		    case JDouble(value) => Map(prefix -> value.toString)
		    case JString(value) => Map(prefix -> value.toString)
		    case _ => Map.empty
		  }

			bind(fromJson(js = data))
		}

	//  /**
	//   * Binds request data to this form, i.e. handles form submission.
	//   *
	//   * @return a copy of this form filled with the new data
	//   */
	//  def bindFromRequest()(implicit request: play.api.mvc.Request[_]): Form[T] = {
	//    bindFromRequest {
	//      (request.body match {
	//        case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
	//        case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
	//        case body: play.api.mvc.AnyContent if body.asJson.isDefined => FormUtils.fromJson(js = body.asJson.get).mapValues(Seq(_))
	//        case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
	//        case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
	//        case body: play.api.libs.json.JsValue => FormUtils.fromJson(js = body).mapValues(Seq(_))
	//        case _ => Map.empty[String, Seq[String]]
	//      }) ++ request.queryString
	//    }
	//  }

	def bindFromRequest(data: Map[String, Seq[String]]): Form[M] = {
		bind {
			data.foldLeft(Map.empty[String, String]) {
				case (s, (key, values)) if key.endsWith("[]") => s ++ values.zipWithIndex.map { case (v, i) => (key.dropRight(2) + "[" + i + "]") -> v }
				case (s, (key, values)) => s + (key -> values.headOption.getOrElse(""))
			}
		}
	}

	/**
	 * Fills this form with a existing value, used for edit forms.
	 *
	 * @param value an existing value of type `T`, used to fill this form
	 * @return a copy of this form filled with the new data
	 */
	def fill(value: mapping.T): Form[M] = {
		val result = mapping.unbind(value)
		this.copy(data = result._1, value = Some(value))
	}

	/**
	 * Fills this form with a existing value, and performs a validation.
	 *
	 * @param value an existing value of type `T`, used to fill this form
	 * @return a copy of this form filled with the new data
	 */
	def fillAndValidate(value: mapping.T): Form[M] = {
		val result = mapping.unbind(value)
		this.copy(data = result._1, errors = result._2, value = Some(value))
	}

	/**
	 * Handles form results. Either the form has errors, or the submission was a success and a
	 * concrete value is available.
	 *
	 * For example:
	 * {{{
	 *   anyForm.bindFromRequest().fold(
	 *      f => redisplayForm(f),
	 *      t => handleValidFormSubmission(t)
	 *   )
	 * }}}
	 *
	 * @tparam R common result type
	 * @param hasErrors a function to handle forms with errors
	 * @param success a function to handle form submission success
	 * @return a result `R`.
	 */
	def fold[R](hasErrors: Form[M] => R, success: M#T => R): R = value.map(success(_)).getOrElse(hasErrors(this))

	def context: FormContext = FormContext(data, key => RepeatedMapping.indexes(key, data), errors)


	/**
	 * Retrieves the first global error, if it exists, i.e. an error without any key.
	 *
	 * @return an error
	 */
	def globalError: Option[FormError] = globalErrors.headOption

	/**
	 * Retrieves all global errors, i.e. errors without a key.
	 *
	 * @return all global errors
	 */
	def globalErrors: Seq[FormError] = errors.filter(_.key.isEmpty)

	/**
	 * Returns `true` if there is an error related to this form.
	 */
	def hasErrors: Boolean = !errors.isEmpty

	/**
	 * Returns `true` if there is a global error related to this form.
	 */
	def hasGlobalErrors: Boolean = !globalErrors.isEmpty

	/**
	 * Returns the concrete value, if the submission was a success.
	 *
	 * Note that this method fails with an Exception if this form as errors.
	 */
	def get: M#T = value.get

	//  /**
	//   * Returns the form errors serialized as Json.
	//   */
	//  def errorsAsJson(implicit lang: play.api.i18n.Lang): play.api.libs.json.JsValue = {
	//
	//    import play.api.libs.json._
	//
	//    Json.toJson(
	//      errors.groupBy(_.key).mapValues { errors =>
	//        errors.map(e => play.api.i18n.Messages(e.message, e.args: _*))
	//      }
	//    )
	//
	//  }

	/**
	 * Adds an error to this form
	 * @param error Error to add
	 * @return a copy of this form with the added error
	 */
	def withError(error: FormError): Form[M] = this.copy(errors = errors :+ error, value = None)

	/**
	 * Convenient overloaded method adding an error to this form
	 * @param key Key of the field having the error
	 * @param message Error message
	 * @param args Error message arguments
	 * @return a copy of this form with the added error
	 */
	def withError(key: String, message: String, args: Any*): Form[M] = withError(FormError(key, message, args))

	/**
	 * Adds a global error to this form
	 * @param message Error message
	 * @param args Error message arguments
	 * @return a copy of this form with the added global error
	 */
	def withGlobalError(message: String, args: Any*): Form[M] = withError(FormError("", message, args))

	/**
	 * Discards this form’s errors
	 * @return a copy of this form without errors
	 */
	def discardingErrors: Form[M] = this.copy(errors = Seq.empty)
}

/**
 * Provides a set of operations for creating `Form` values.
 */
object Form {

	/**
	 * Creates a new form from a mapping.
	 *
	 * For example:
	 * {{{
	 * import play.api.data._
	 * import format.Formats._
	 *
	 * val userForm = Form(
	 *   tuple(
	 *     of[String],
	 *     of[Int],
	 *     of[String]
	 *   )
	 * )
	 * }}}
	 *
	 * @param mapping the form mapping
	 * @return a form definition
	 */
	def apply[M <: Mapping[M]](mapping: M): Form[M] = Form(mapping, Map.empty, Nil, None)

}


/**
 * A form error.
 *
 * @param key The error key (should be associated with a field using the same key).
 * @param message The form message (often a simple message key needing to be translated).
 * @param args Arguments used to format the message.
 */
case class FormError(key: String, message: String, args: Seq[Any] = Nil) {

	/**
	 * Copy this error with a new Message.
	 *
	 * @param message The new message.
	 */
	def withMessage(message: String): FormError = copy(message = message)

}

