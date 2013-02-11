package org.ladderframework.html.form

import scala.annotation.implicitNotFound
import org.ladderframework.Utils

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
case class Form[M <: Mapping](mapping: M, data: Map[String, String], errors: Seq[FormError], value: Option[M#T]) {
	
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

	//  /**
	//   * Binds data to this form, i.e. handles form submission.
	//   *
	//   * @param data Json data to submit
	//   * @return a copy of this form, filled with the new data
	//   */
	//  def bind(data: play.api.libs.json.JsValue): Form[T] = bind(FormUtils.fromJson(js = data))

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

	/**
	 * Retrieves a field.
	 *
	 * For example:
	 * {{{
	 * val usernameField = field
	 * }}}
	 *
	 * @param key the field name
	 * @return the field, returned even if the field does not exist
	 */
	def context: FormContext = FormContext(data.get, errors)

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
	def apply[M <: Mapping](mapping: M): Form[M] = Form(mapping, Map.empty, Nil, None)

}

private[form] object FormUtils {

	//  import play.api.libs.json._
	//
	//  def fromJson(prefix: String = "", js: JsValue): Map[String, String] = js match {
	//    case JsObject(fields) => {
	//      fields.map { case (key, value) => fromJson(Option(prefix).filterNot(_.isEmpty).map(_ + ".").getOrElse("") + key, value) }.foldLeft(Map.empty[String, String])(_ ++ _)
	//    }
	//    case JsArray(values) => {
	//      values.zipWithIndex.map { case (value, i) => fromJson(prefix + "[" + i + "]", value) }.foldLeft(Map.empty[String, String])(_ ++ _)
	//    }
	//    case JsNull => Map.empty
	//    case JsUndefined(_) => Map.empty
	//    case JsBoolean(value) => Map(prefix -> value.toString)
	//    case JsNumber(value) => Map(prefix -> value.toString)
	//    case JsString(value) => Map(prefix -> value.toString)
	//  }

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

trait NestedMapping extends Mapping{
	type S
	def mappings: S
}

/**
 * A mapping is a two-way binder to handle a form field.
 */
trait Mapping {
	self =>
		
	type T

	/**
	 * The field key.
	 */
	val key: String

	/**
	 * The Format expected for this field, if it exists.
	 */
	val format: Option[(String, Seq[Any])] = None

	/**
	 * The constraints associated with this field.
	 */
	val constraints: Seq[Constraint[T]]

	/**
	 * Binds this field, i.e. construct a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `T` or a set of errors, if the binding failed
	 */
	def bind(data: Map[String, String]): Either[Seq[FormError], T]

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if the unbinding failed
	 */
	def unbind(value: T): (Map[String, String], Seq[FormError])

	/**
	 * Constructs a new Mapping based on this one, adding a prefix to the key.
	 *
	 * @param prefix the prefix to add to the key
	 * @return the same mapping, with only the key changed
	 */
	def withPrefix(prefix: String): Mapping{type T = self.T}

	/**
	 * Constructs a new Mapping based on this one, by adding new constraints.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying(required) )
	 * }}}
	 *
	 * @param constraints the constraints to add
	 * @return the new mapping
	 */
	def verifying(constraints: Constraint[T]*): Mapping{type T = self.T}

	/**
	 * Constructs a new Mapping based on this one, by adding a new ad-hoc constraint.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying {_.grouped(2).size == 5})
	 * }}}
	 *
	 * @param constraint a function describing the constraint that returns `false` on failure
	 * @return the new mapping
	 */
	def verifying(constraint: (T => Boolean)): Mapping{type T = self.T} = verifying("error.unknown", constraint)

	/**
	 * Constructs a new Mapping based on this one, by adding a new ad-hoc constraint.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying("Bad phone number", {_.grouped(2).size == 5}))
	 * }}}
	 *
	 * @param error The error message used if the constraint fails
	 * @param constraint a function describing the constraint that returns `false` on failure
	 * @return the new mapping
	 */
	def verifying(error: => String, constraint: (self.T => Boolean)): Mapping{type T = self.T} = {
		verifying(Constraint { t: T =>
			if (constraint(t)) Valid else Invalid(Seq(ValidationError(error)))
		})
	}

	/**
	 * Transform this Mapping[T] to a Mapping[B].
	 *
	 * @tparam B The type of the new mapping.
	 * @param f1 Transform value of T to a value of B
	 * @param f2 Transform value of B to a value of T
	 */
	def transform[B](f1: self.T => B, f2: B => self.T) = WrappedMapping[T, B](self, f1, f2)

	// Internal utilities

	protected def addPrefix(prefix: String) = {
		Option(prefix).filterNot(_.isEmpty).map(p => p + Option(key).filterNot(_.isEmpty).map("." + _).getOrElse(""))
	}

	protected def applyConstraints(t: T): Either[Seq[FormError], T] = {
		Right(t).right.flatMap { v =>
			Option(collectErrors(v)).filterNot(_.isEmpty).toLeft(v)
		}
	}

	protected def collectErrors(t: T): Seq[FormError] = {
		constraints.map(_(t)).collect {
			case Invalid(errors) => errors.toSeq
		}.flatten.map(ve => FormError(key, ve.message, ve.args))
	}

}

/**
 * A mapping wrapping another existing mapping with transformation functions.
 *
 * @param wrapped Existing wrapped mapping
 * @param f1 Transformation function from A to B
 * @param f2 Transformation function from B to A
 * @param additionalConstraints Additional constraints of type B
 */
case class WrappedMapping[A, B](wrapped: Mapping{type T = A}, f1: A => B, f2: B => A, val additionalConstraints: Seq[Constraint[B]] = Nil) extends NestedMapping {
	type T = B
	type S = Mapping{type T = A}

	/**
	 * The field key.
	 */
	val key = wrapped.key

	/**
	 * Sub-mappings (these can be seen as sub-keys).
	 */
	val mappings = wrapped

	/**
	 * The Format expected for this field, if it exists.
	 */
	override val format = wrapped.format

	/**
	 * The constraints associated with this field.
	 */
	val constraints: Seq[Constraint[B]] = wrapped.constraints.map { constraintOfT =>
		Constraint[B](constraintOfT.name, constraintOfT.args) { b =>
			constraintOfT(f2(b))
		}
	} ++ additionalConstraints

	/**
	 * Binds this field, i.e. construct a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `B` or a set of errors, if the binding failed
	 */
	def bind(data: Map[String, String]): Either[Seq[FormError], B] = {
		wrapped.bind(data).right.map(t => f1(t)).right.flatMap(applyConstraints)
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if the unbinding failed
	 */
	def unbind(value: B): (Map[String, String], Seq[FormError]) = {
		(wrapped.unbind(f2(value))._1, collectErrors(value))
	}

	/**
	 * Constructs a new Mapping based on this one, adding a prefix to the key.
	 *
	 * @param prefix the prefix to add to the key
	 * @return the same mapping, with only the key changed
	 */
	def withPrefix(prefix: String): Mapping{type T = B} = {
		copy(wrapped = wrapped.withPrefix(prefix))
	}

	/**
	 * Constructs a new Mapping based on this one, by adding new constraints.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying(required) )
	 * }}}
	 *
	 * @param constraints the constraints to add
	 * @return the new mapping
	 */
	def verifying(constraints: Constraint[B]*): Mapping{type T = B} = copy(additionalConstraints = additionalConstraints ++ constraints)

}

/**
 * Provides a set of operations related to `RepeatedMapping` values.
 */
object RepeatedMapping {

	/**
	 * Computes the available indexes for the given key in this set of data.
	 */
	def indexes(key: String, data: Map[String, String]): Seq[Int] = {
		val KeyPattern = ("^" + java.util.regex.Pattern.quote(key) + """\[(\d+)\].*$""").r
		data.toSeq.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
	}

}

/**
 * A mapping for repeated elements.
 *
 * @param wrapped The wrapped mapping
 */
case class RepeatedMapping[RT](wrapped: Mapping{type T = RT}, val key: String = "", val constraints: Seq[Constraint[List[RT]]] = Nil) extends NestedMapping {

	type T = List[RT]
	type S = Mapping{type T = RT}
	
	/**
	 * The Format expected for this field, if it exists.
	 */
	override val format: Option[(String, Seq[Any])] = wrapped.format

	/**
	 * Constructs a new Mapping based on this one, by adding new constraints.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying(required) )
	 * }}}
	 *
	 * @param constraints the constraints to add
	 * @return the new mapping
	 */
	def verifying(addConstraints: Constraint[T]*) = {
		this.copy(constraints = constraints ++ addConstraints.toSeq)
	}

	/**
	 * Binds this field, i.e. construct a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `List[T]` or a set of errors, if the binding failed
	 */
	def bind(data: Map[String, String]): Either[Seq[FormError], List[RT]] = {
		val allErrorsOrItems: Seq[Either[Seq[FormError], RT]] = RepeatedMapping.indexes(key, data).map(i => wrapped.withPrefix(key + "[" + i + "]").bind(data))
		if (allErrorsOrItems.forall(_.isRight)) {
			Right(allErrorsOrItems.map(_.right.get).toList).right.flatMap(applyConstraints)
		} else {
			Left(allErrorsOrItems.collect { case Left(errors) => errors }.flatten)
		}
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if the unbinding failed
	 */
	def unbind(value: List[RT]): (Map[String, String], Seq[FormError]) = {
		val (datas, errors) = value.zipWithIndex.map { case (t, i) => wrapped.withPrefix(key + "[" + i + "]").unbind(t) }.unzip
		(datas.foldLeft(Map.empty[String, String])(_ ++ _), errors.flatten ++ collectErrors(value))
	}

	/**
	 * Constructs a new Mapping based on this one, adding a prefix to the key.
	 *
	 * @param prefix the prefix to add to the key
	 * @return the same mapping, with only the key changed
	 */
	def withPrefix(prefix: String): Mapping{type T = List[RT]} = {
		addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)
	}

	/**
	 * Sub-mappings (these can be seen as sub-keys).
	 */
	val mappings = wrapped

}

/**
 * A mapping for optional elements
 *
 * @param wrapped the wrapped mapping
 */
case class OptionalMapping[OT](wrapped: Mapping{type T = OT}, val constraints: Seq[Constraint[Option[OT]]] = Nil) extends NestedMapping {

	type T = Option[OT]
	type S = Mapping{type T = OT}
	
	override val format: Option[(String, Seq[Any])] = wrapped.format

	/**
	 * The field key.
	 */
	val key = wrapped.key

	/**
	 * Constructs a new Mapping based on this one, by adding new constraints.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying(required) )
	 * }}}
	 *
	 * @param constraints the constraints to add
	 * @return the new mapping
	 */
	def verifying(addConstraints: Constraint[Option[OT]]*): Mapping{
		type T = Option[OT]
		type S = Mapping{type T = OT}
	} = {
		this.copy(constraints = constraints ++ addConstraints.toSeq)
	}

	/**
	 * Binds this field, i.e. constructs a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `T` or a set of error if the binding failed
	 */
	def bind(data: Map[String, String]): Either[Seq[FormError], Option[OT]] = {
		data.keys.filter(p => p == key || p.startsWith(key + ".") || p.startsWith(key + "[")).map(k => data.get(k).filterNot(_.isEmpty)).collect { case Some(v) => v }.headOption.map { _ =>
			wrapped.bind(data).right.map(Some(_))
		}.getOrElse {
			Right(None)
		}.right.flatMap(applyConstraints)
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value The value to unbind.
	 * @return Either the plain data or a set of error if the unbinding failed.
	 */
	def unbind(value: Option[OT]): (Map[String, String], Seq[FormError]) = {
		val errors = collectErrors(value)
		value.map(wrapped.unbind(_)).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
	}

	/**
	 * Constructs a new Mapping based on this one, adding a prefix to the key.
	 *
	 * @param prefix the prefix to add to the key
	 * @return the same mapping, with only the key changed
	 */
	def withPrefix(prefix: String): Mapping{
		type T = Option[OT]
		type S = Mapping{type T = OT}
	} = {
		copy(wrapped = wrapped.withPrefix(prefix))
	}

	/** Sub-mappings (these can be seen as sub-keys). */
	val mappings = wrapped

}

/**
 * A mapping for a single field.
 *
 * @param key the field key
 * @param constraints the constraints associated with this field.
 */
case class FieldMapping[FT](val key: String = Utils.uuid, val constraints: Seq[Constraint[FT]] = Nil)(implicit val binder: Formatter[FT]) extends Mapping {
	
	type T = FT

	/**
	 * The Format expected for this field, if it exists.
	 */
	override val format: Option[(String, Seq[Any])] = binder.format

	/**
	 * Constructs a new Mapping based on this one, by adding new constraints.
	 *
	 * For example:
	 * {{{
	 *   import play.api.data._
	 *   import validation.Constraints._
	 *
	 *   Form("phonenumber" -> text.verifying(required) )
	 * }}}
	 *
	 * @param constraints the constraints to add
	 * @return the new mapping
	 */
	def verifying(addConstraints: Constraint[T]*) = {
		this.copy(constraints = constraints ++ addConstraints.toSeq)
	}

	/**
	 * Changes the binder used to handle this field.
	 *
	 * @param binder the new binder to use
	 * @return the same mapping with a new binder
	 */
	def as(binder: Formatter[T]) = {
		this.copy()(binder)
	}

	/**
	 * Binds this field, i.e. constructs a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `T` or a set of errors, if binding failed
	 */
	def bind(data: Map[String, String]): Either[Seq[FormError], T] = {
		binder.bind(key, data).right.flatMap { applyConstraints(_) }
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if unbinding failed
	 */
	def unbind(value: T): (Map[String, String], Seq[FormError]) = {
		binder.unbind(key, value) -> collectErrors(value)
	}

	/**
	 * Constructs a new Mapping based on this one, adding a prefix to the key.
	 *
	 * @param prefix the prefix to add to the key
	 * @return the same mapping, with only the key changed
	 */
	def withPrefix(prefix: String) = {
		addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)
	}

	/** Sub-mappings (these can be seen as sub-keys). */
	val mappings = Seq(this)

}

/**
 * Common helper methods for all object mappings - mappings including several fields.
 */
trait ObjectMapping {

	/**
	 * Merges the result of two bindings.
	 *
	 * @see bind()
	 */
	def merge2(a: Either[Seq[FormError], Seq[Any]], b: Either[Seq[FormError], Seq[Any]]): Either[Seq[FormError], Seq[Any]] = (a, b) match {
		case (Left(errorsA), Left(errorsB)) => Left(errorsA ++ errorsB)
		case (Left(errorsA), Right(_)) => Left(errorsA)
		case (Right(_), Left(errorsB)) => Left(errorsB)
		case (Right(a), Right(b)) => Right(a ++ b)
	}

	/**
	 * Merges the result of multiple bindings.
	 *
	 * @see bind()
	 */
	def merge(results: Either[Seq[FormError], Any]*): Either[Seq[FormError], Seq[Any]] = {
		val all: Seq[Either[Seq[FormError], Seq[Any]]] = results.map(_.right.map(Seq(_)))
		all.fold(Right(Nil)) { (s, i) => merge2(s, i) }
	}

}

