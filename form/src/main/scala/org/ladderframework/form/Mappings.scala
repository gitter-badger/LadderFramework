package org.ladderframework.form

import org.ladderframework.utils._

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
	def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], T]

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if the unbinding failed
	 */
	def unbind(value: T, prefix: String): (Map[String, String], Seq[FormError])

	def applyConstraints(t: T, prefix: String): Either[Seq[FormError], T] = {
		Right(t).right.flatMap { v =>
			Option(collectErrors(v, prefix)).filterNot(_.isEmpty).toLeft(v)
		}
	}

	def collectErrors(t: T, prefix: String): Seq[FormError] = {
		constraints.map(_(t)).collect {
			case Invalid(errors) => errors.toSeq
		}.flatten.map(ve => FormError(prefix + key, ve.message, ve.args))
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
case class WrappedMapping[A, B, M <: Mapping{type T = A}](wrapped: M{type T = A}, f1: A => B, f2: B => A, val additionalConstraints: Seq[Constraint[B]] = Nil) extends NestedMapping {
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
	def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], B] = {
		wrapped.bind(data, prefix).right.map(t => f1(t)).right.flatMap(applyConstraints(_, prefix))
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if the unbinding failed
	 */
	def unbind(value: B, prefix: String): (Map[String, String], Seq[FormError]) = {
		(wrapped.unbind(f2(value), prefix)._1, collectErrors(value, prefix))
	}

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
case class RepeatedMapping[RT, M <: Mapping{type T = RT}](wrapped: M{type T = RT}, key: String, constraints: Seq[Constraint[List[RT]]] = Nil) extends NestedMapping {

	type T = List[RT]
	type S = M
	
	/**
	 * The Format expected for this field, if it exists.
	 */
	override val format: Option[(String, Seq[Any])] = wrapped.format

	/**
	 * Binds this field, i.e. construct a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `List[T]` or a set of errors, if the binding failed
	 */
	def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], List[RT]] = {
		val allErrorsOrItems: Seq[Either[Seq[FormError], RT]] = RepeatedMapping.indexes(prefix + key, data).map(i => wrapped.bind(data, s"$prefix$key[$i]."))
		if (allErrorsOrItems.forall(_.isRight)) {
			Right(allErrorsOrItems.map(_.right.get).toList).right.flatMap(applyConstraints(_, prefix))
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
	override def unbind(value: List[RT], prefix: String): (Map[String, String], Seq[FormError]) = {
		val (datas, errors) = value.zipWithIndex.map { case (t, i) => wrapped.unbind(t, s"$prefix$key[$i].") }.unzip
		(datas.foldLeft(Map.empty[String, String])(_ ++ _), errors.flatten ++ collectErrors(value, prefix))
	}

//	/**
//	 * Constructs a new Mapping based on this one, adding a prefix to the key.
//	 *
//	 * @param prefix the prefix to add to the key
//	 * @return the same mapping, with only the key changed
//	 */
//	def withPrefix(prefix: String) = {
//		addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)
//	}

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
case class OptionalMapping[OT, M <: Mapping{type T = OT}](wrapped: M{type T = OT}, val constraints: Seq[Constraint[Option[OT]]] = Nil) extends NestedMapping {

	override type T = Option[OT]
	override type S = M{type T = OT}
	
	override val format: Option[(String, Seq[Any])] = wrapped.format

	/**
	 * The field key.
	 */
	val key = wrapped.key

	/**
	 * Binds this field, i.e. constructs a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `T` or a set of error if the binding failed
	 */
	def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], Option[OT]] = {
    val key = prefix + this.key
		data.keys.filter(p => p == key || p.startsWith(key + ".") || p.startsWith(key + "[")).map(k => data.get(k).filterNot(_.isEmpty)).collect { case Some(v) => v }.headOption.map { _ =>
			wrapped.bind(data, prefix).right.map(Some(_))
		}.getOrElse {
			Right(None)
		}.right.flatMap(applyConstraints(_, prefix))
	}

	def unbind(value: Option[OT], prefix: String): (Map[String, String], Seq[FormError]) = {
		val errors = collectErrors(value, prefix)
		value.map(wrapped.unbind(_, prefix)).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
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
case class FieldMapping[FT](key: String = uuid, constraints: Seq[Constraint[FT]] = Nil)(implicit val binder: Formatter[FT]) extends Mapping {
	
	type T = FT

	/**
	 * The Format expected for this field, if it exists.
	 */
	override val format: Option[(String, Seq[Any])] = binder.format

	/**
	 * Binds this field, i.e. constructs a concrete value from submitted data.
	 *
	 * @param data the submitted data
	 * @return either a concrete value of type `T` or a set of errors, if binding failed
	 */
	override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], T] = {
		binder.bind(prefix + key, data).right.flatMap { applyConstraints(_, prefix) }
	}

	/**
	 * Unbinds this field, i.e. transforms a concrete value to plain data.
	 *
	 * @param value the value to unbind
	 * @return either the plain data or a set of errors, if unbinding failed
	 */
	override def unbind(value: T, prefix: String): (Map[String, String], Seq[FormError]) = {
		binder.unbind(prefix + key, value) -> collectErrors(value, prefix)
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
  
  def key: String
  
  lazy val optKey = if(key.isEmpty()) "" else key + "."

}

