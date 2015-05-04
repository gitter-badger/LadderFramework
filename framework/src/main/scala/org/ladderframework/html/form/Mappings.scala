package org.ladderframework.html.form

import org.ladderframework.Utils

trait NestedMapping[M <: NestedMapping[M]] extends Mapping[M]{
	type S
	def mappings: S
}

/**
 * A mapping is a two-way binder to handle a form field.
 */
trait Mapping[M <: Mapping[M]] {
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
	def withPrefix(prefix: String): M // Mapping{type T = self.T}

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
	def verifying(constraints: Constraint[T]*): M

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
	def verifying(constraint: (T => Boolean)): M = verifying("error.unknown", constraint)

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
	def verifying(error: => String, constraint: (self.T => Boolean)): M = {
		verifying(Constraint { t: T =>
			if (constraint(t)) Valid else Invalid(Seq(ValidationError(error)))
		})
	}

	protected def addPrefix(prefix: String): Option[String] = {
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
case class WrappedMapping[A, B, M <: Mapping[M]{type T = A}](wrapped: M, f1: A => B, f2: B => A, val additionalConstraints: Seq[Constraint[B]] = Nil) extends NestedMapping[WrappedMapping[A, B, M]] {
	type T = B
	type S = M

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
	def withPrefix(prefix: String) = copy(wrapped = wrapped.withPrefix(prefix))

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
	def verifying(constraints: Constraint[B]*) = copy(additionalConstraints = additionalConstraints ++ constraints)

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
case class RepeatedMapping[RT, M <: Mapping[M]{type T = RT}](wrapped: M{type T = RT}, val key: String = Utils.uuid, val constraints: Seq[Constraint[List[RT]]] = Nil) extends NestedMapping[RepeatedMapping[RT, M]] {

	type T = List[RT]
	type S = M{type T = RT}
	
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
	def withPrefix(prefix: String) = {
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
case class OptionalMapping[OT, M <: Mapping[M]{type T = OT}](wrapped: M, val constraints: Seq[Constraint[Option[OT]]] = Nil) extends NestedMapping[OptionalMapping[OT, M]] {

	type T = Option[OT]
	type S = M
	
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
	def verifying(addConstraints: Constraint[Option[OT]]*) = {
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
	def withPrefix(prefix: String) = copy(wrapped = wrapped.withPrefix(prefix))

	/** Sub-mappings (these can be seen as sub-keys). */
	val mappings = wrapped

}

/**
 * A mapping for a single field.
 *
 * @param key the field key
 * @param constraints the constraints associated with this field.
 */
case class FieldMapping[FT](val key: String = Utils.uuid, val constraints: Seq[Constraint[FT]] = Nil)(implicit val binder: Formatter[FT]) extends Mapping[FieldMapping[FT]] {
	
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
	def mrg[A, B](a: Either[Seq[FormError], A], b: Either[Seq[FormError], B]): Either[Seq[FormError], (A, B)] = (a, b) match {
		case (Left(errorsA), Left(errorsB)) => Left(errorsA ++ errorsB)
		case (Left(errorsA), Right(_)) => Left(errorsA)
		case (Right(_), Left(errorsB)) => Left(errorsB)
		case (Right(a), Right(b)) => Right(a -> b)
	}
	
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

