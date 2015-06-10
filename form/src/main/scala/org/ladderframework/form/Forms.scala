package org.ladderframework.form

import scala.annotation._

/**
 * Contains data manipulation helpers (typically HTTP form handling)
 *
 * {{{
 * import play.api.data._
 * import play.api.data.Forms._
 *
 * val taskForm = Form(
 *   of(Task.apply _, Task.unapply _)(
 *     text(minLength = 3),
 *     date("yyyy-MM-dd"),
 *     boolean
 *   )
 * )
 * }}}
 *
 */
object named {

	implicit class TransformWrapper[A, M <: Mapping{type T = A}](mapping: M{type T = A}){
  	/**
		 * Transform this Mapping[A] to a Mapping[B].
		 *
		 * @tparam B The type of the new mapping.
		 * @param f1 Transform value of A to a value of B
		 * @param f2 Transform value of B to a value of A
		 */
		def transform[B](f1: A => B, f2: B => A) = WrappedMapping[A, B, M](mapping, f1, f2)
  }
  
  case class NameableMapping[M <: Mapping](toMapping: String => M){
    type MM = M
    def apply(in: String): MM = toMapping(in)
  }
  
  implicit class NameString(in: String){
    def -->[M <: Mapping] (nm: NameableMapping[M]): nm.MM = nm(in)
  }
  
  implicit def string2Mapping[A](name: String)(implicit nm:NameableMapping[FieldMapping[A]]):nm.MM = nm.toMapping(name)
  implicit def touple2Mapping[M <: Mapping](t: (String, NameableMapping[M])): t._2.MM = t._2(t._1)
  
  /**
   * Creates a Mapping of type `T`.
   *
   * For example:
   * {{{
   * Form(of[String])
   * }}}
   *
   * @tparam T the mapping type
   * @return a mapping for a simple field
   */
	implicit def of[T](implicit binder: Formatter[T]):NameableMapping[FieldMapping[T]] = NameableMapping(FieldMapping(_)) 

  def of[T](constraints: Constraint[T]*)(implicit binder: Formatter[T]): NameableMapping[FieldMapping[T]] = NameableMapping(FieldMapping(_, constraints = constraints.toSeq))

  /**
   * Creates a Mapping of type `T`.
   *
   * For example:
   * {{{
   * Form(
   *   mapping(
   *     of[String]
   *   )(User.apply, User.unapply)
   * )
   * }}}
   *
   * @tparam T the mapped type
   * @param apply A function able to create a value of T from a value of A1 (If T is case class you can use its own apply function)
   * @param unapply A function able to create A1 from a value of T (If T is a case class you can use its own unapply function)
   * @return a mapping for type `T`
   */
  
  def mapping[R, A1, M1 <: Mapping](apply: A1 => R)(unapply: R => Option[(A1)])(a1:M1{type T = A1}) = {
    NameableMapping(ObjectMapping1(apply, unapply, a1, _))
  }

  def mapping[R, A1, A2, M1 <: Mapping, M2 <: Mapping](apply: Function2[A1, A2, R])(unapply: Function1[R, Option[(A1, A2)]])(a1:M1{type T = A1}, a2:M2{type T = A2}) = {
    NameableMapping(ObjectMapping2(apply, unapply, a1, a2, _))
  }

  def mapping[R, A1, A2, A3, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping](apply: Function3[A1, A2, A3, R])(unapply: Function1[R, Option[(A1, A2, A3)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}) = {
    NameableMapping(ObjectMapping3(apply, unapply, a1, a2, a3,_))
  }

  def mapping[R, A1, A2, A3, A4, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping](apply: Function4[A1, A2, A3, A4, R])(unapply: Function1[R, Option[(A1, A2, A3, A4)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}) = {
    NameableMapping(ObjectMapping4(apply, unapply, a1, a2, a3, a4,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping](apply: Function5[A1, A2, A3, A4, A5, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}) = {
    NameableMapping(ObjectMapping5(apply, unapply, a1, a2, a3, a4, a5,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping](apply: Function6[A1, A2, A3, A4, A5, A6, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}) = {
    NameableMapping(ObjectMapping6(apply, unapply, a1, a2, a3, a4, a5, a6,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping](apply: Function7[A1, A2, A3, A4, A5, A6, A7, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}) = {
    NameableMapping(ObjectMapping7(apply, unapply, a1, a2, a3, a4, a5, a6, a7,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping](apply: Function8[A1, A2, A3, A4, A5, A6, A7, A8, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}) = {
    NameableMapping(ObjectMapping8(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping](apply: Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}) = {
    NameableMapping(ObjectMapping9(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping](apply: Function10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}) = {
    NameableMapping(ObjectMapping10(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping](apply: Function11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}) = {
    NameableMapping(ObjectMapping11(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping](apply: Function12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}) = {
    NameableMapping(ObjectMapping12(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping](apply: Function13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}) = {
    NameableMapping(ObjectMapping13(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping](apply: Function14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}) = {
    NameableMapping(ObjectMapping14(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping](apply: Function15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}) = {
    NameableMapping(ObjectMapping15(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping](apply: Function16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}) = {
    NameableMapping(ObjectMapping16(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping, M17 <: Mapping](apply: Function17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}, a17:M17{type T = A17}) = {
    NameableMapping(ObjectMapping17(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17,_))
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping, M17 <: Mapping, M18 <: Mapping](apply: Function18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}, a17:M17{type T = A17}, a18:M18{type T = A18}) = {
    NameableMapping(ObjectMapping18(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18,_))
  }


  import Form._

  /**
   * Constructs a simple mapping for a text field.
   *
   * For example:
   * {{{
   * Form(text)
   * }}}
   */
  def text(implicit binder: Formatter[String]): NameableMapping[FieldMapping[String]] = of[String]
  def text(constraints: Constraint[String]*)(implicit binder: Formatter[String]): NameableMapping[FieldMapping[String]] = of[String](constraints = constraints:_*)

  /**
   * Constructs a simple mapping for required text field.
   *
   * Note that all field are always required to be present in the form unless
   * there are marked as optional explicitely. But a nonEmptyText defines text
   * field that must not be empty, even if present in the form.
   *
   * Example:
   * {{{
   * Form(nonEmptyText)
   * }}}
   */
  def nonEmptyText: NameableMapping[FieldMapping[String]] = text(Constraints.nonEmpty)

  /**
   * Constructs a simple mapping for a text field.
   *
   * For example:
   * {{{
   * Form(text(minLength=3))
   * }}}
   *
   * @param minLength minimum text length
   * @param maxLength maximum text length
   */
  def text(minLength: Int = 0, maxLength: Int = Int.MaxValue):NameableMapping[FieldMapping[String]] = (minLength, maxLength) match {
    case (0, Int.MaxValue) => text
    case (min, Int.MaxValue) => text(Constraints.minLength(min))
    case (0, max) => text(Constraints.maxLength(max))
    case (min, max) => text(Constraints.minLength(min), Constraints.maxLength(max))
  }

  /**
   * Constructs a simple mapping for required text field.
   *
   * Example:
   * {{{
   * Form(nonEmptyText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  def nonEmptyText(minLength: Int = 0, maxLength: Int = Int.MaxValue): NameableMapping[FieldMapping[String]] = (minLength, maxLength) match {
    case (0, Int.MaxValue) => text(Constraints.nonEmpty)
    case (min, Int.MaxValue) => text(Constraints.minLength(min), Constraints.nonEmpty)
    case (0, max) => text(Constraints.maxLength(max), Constraints.nonEmpty)
    case (min, max) => text(Constraints.minLength(min), Constraints.maxLength(max), Constraints.nonEmpty)
  }

  /**
   * Constructs a simple mapping for a numeric field.
   *
   * For example:
   * {{{
   * Form(number)
   * }}}
   */
  def number: NameableMapping[FieldMapping[Int]] = of[Int]
	def number(contraints: Constraint[Int]*): NameableMapping[FieldMapping[Int]] = of[Int](constraints = contraints:_*)

  /**
   * Constructs a simple mapping for a numeric field (using a Long type behind).
   *
   * For example:
   * {{{
   * Form(longNumber)
   * }}}
   */
  def longNumber: NameableMapping[FieldMapping[Long]] = of[Long]
  def longNumber(constraints: Constraint[Long]*): NameableMapping[FieldMapping[Long]] = of[Long](constraints = constraints:_*)

  /**
   * Constructs a simple mapping for a numeric field.
   *
   * For example:
   * {{{
   * Form(number(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   */
  def number(min: Int = Int.MinValue, max: Int = Int.MaxValue): NameableMapping[FieldMapping[Int]] = (min, max) match {
    case (Int.MinValue, Int.MaxValue) => number
    case (min, Int.MaxValue) => number(Constraints.min(min))
    case (Int.MinValue, max) => number(Constraints.max(max))
    case (min, max) => number(Constraints.min(min), Constraints.max(max))
  }

  /**
   * Constructs a simple mapping for a numeric field (using a Long type behind).
   *
   * For example:
   * {{{
   * Form(longNumber(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   */
  def longNumber(min: Long = Long.MinValue, max: Long = Long.MaxValue): NameableMapping[FieldMapping[Long]] = (min, max) match {
    case (Long.MinValue, Long.MaxValue) => longNumber
    case (min, Long.MaxValue) => longNumber(Constraints.min(min))
    case (Long.MinValue, max) => longNumber(Constraints.max(max))
    case (min, max) => longNumber(Constraints.min(min), Constraints.max(max))
  }

  /**
   * Constructs a simple mapping for a date field.
   *
   * For example:
   * {{{
   * Form(date)
   * }}}
   */
  val date: NameableMapping[FieldMapping[java.util.Date]] = of[java.util.Date]

  /**
   * Define a fixed value in a mapping.
   * This mapping will not participate to the binding.
   *
   * @param value As we ignore this parameter in binding/unbinding we have to provide a default value.
   */
  def ignored[A](value: A): NameableMapping[FieldMapping[A]] = of(Formatter.ignoredFormat(value))

  /**
   * Defines an optional mapping.
   *
   * {{{
   * Form(
   *   optional(text)
   * )
   * }}}
   *
   * @param mapping The mapping to make optional.
   */
  def optional[A, M <: Mapping{type T = A}](mapping: NameableMapping[M{type T = A}]):NameableMapping[OptionalMapping[A,M]] = NameableMapping(name => OptionalMapping[A, M](mapping.toMapping(name)))

  /**
   * Defines an default mapping, if the parameter is not present, provide a default value.
   *
   * {{{
   * Form(
   *   default(text, "The default text")
   * )
   * }}}
   *
   * @param mapping The mapping to make optional.
   * @param value The default value when mapping and the field is not present.
   */
  def default[A, M <: Mapping{type T = A}](mapping: M{type T = A}, value:A) = OptionalMapping[A, M](mapping).transform[A](_.getOrElse(value), Some(_))

  /**
   * Defines a repeated mapping.
   * {{{
   * Form(
   *   list(text)
   * )
   * }}}
   *
   * @param mapping The mapping to make repeated.
   */
  def list[A, M <: Mapping{type T = A}](mapping: NameableMapping[M{type T = A}]): NameableMapping[RepeatedMapping[A,M]] = NameableMapping(RepeatedMapping[A, M](mapping.toMapping(""), _))

  /**
   * Defines a repeated mapping.
   * {{{
   * Form(
   *   seq(text)
   * )
   * }}}
   * 
   * @param mapping The mapping to make repeated.
   */
  def seq[A, M <: Mapping{type T = A}](mapping: M{type T = A}): NameableMapping[WrappedMapping[List[A], Seq[A], RepeatedMapping[A, M]]] = NameableMapping(RepeatedMapping[A, M](mapping, _).transform[Seq[A]](_.toSeq, _.toList))

  /**
   * Constructs a simple mapping for a date field.
   *
   * For example:
   * {{{
   *   Form(date("dd-MM-yyyy"))
   * }}}
   *
   * @param pattern the date pattern, as defined in `java.text.SimpleDateFormat`
   */
  def date(pattern: String): NameableMapping[FieldMapping[java.util.Date]] = of[java.util.Date]

  /**
   * Constructs a simple mapping for an e-mail field.
   *
   * For example:
   * {{{
   *   Form(email)
   * }}}
   */
  def email: NameableMapping[FieldMapping[String]] = of[String](Constraints.pattern(
    """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r,
    "constraint.email",
    "error.email"))

  /**
   * Constructs a simple mapping for a Boolean field, such as a check-box.
   *
   * For example:
   * {{{
   *   Form(boolean)
   * }}}
   */
  def boolean: NameableMapping[FieldMapping[Boolean]] = of[Boolean]

  def checked(msg: String): NameableMapping[FieldMapping[Boolean]] = of[Boolean](Constraint((bool: Boolean) => if(bool) Valid else Invalid(msg) ))
  
}

object secure {
  
  implicit class TransformWrapper[A, M <: Mapping{type T = A}](mapping: M{type T = A}){
    /**
     * Transform this Mapping[A] to a Mapping[B].
     *
     * @tparam B The type of the new mapping.
     * @param f1 Transform value of A to a value of B
     * @param f2 Transform value of B to a value of A
     */
    def transform[B](f1: A => B, f2: B => A) = WrappedMapping[A, B, M](mapping, f1, f2)
  }
  
  /**
   * Creates a Mapping of type `T`.
   *
   * For example:
   * {{{
   * Form(of[String])
   * }}}
   *
   * @tparam T the mapping type
   * @return a mapping for a simple field
   */
  def of[T](implicit binder: Formatter[T]) = FieldMapping[T]()
  def of[T](constraints: Constraint[T]*)(implicit binder: Formatter[T]) = FieldMapping[T](constraints = constraints.toSeq)(binder)

  /**
   * Creates a Mapping of type `T`.
   *
   * For example:
   * {{{
   * Form(
   *   mapping(
   *     of[String]
   *   )(User.apply, User.unapply)
   * )
   * }}}
   *
   * @tparam T the mapped type
   * @param apply A function able to create a value of T from a value of A1 (If T is case class you can use its own apply function)
   * @param unapply A function able to create A1 from a value of T (If T is a case class you can use its own unapply function)
   * @return a mapping for type `T`
   */
  import _root_.org.ladderframework.utils._
  def mapping[R, A1, M1 <: Mapping](apply: A1 => R)(unapply: R => Option[(A1)])(a1:M1{type T = A1}) = {
    ObjectMapping1(apply, unapply, a1, uuid)
  }

  def mapping[R, A1, A2, M1 <: Mapping, M2 <: Mapping](apply: Function2[A1, A2, R])(unapply: Function1[R, Option[(A1, A2)]])(a1:M1{type T = A1}, a2:M2{type T = A2}) = {
    ObjectMapping2(apply, unapply, a1, a2, uuid)
  }

  def mapping[R, A1, A2, A3, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping](apply: Function3[A1, A2, A3, R])(unapply: Function1[R, Option[(A1, A2, A3)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}) = {
    ObjectMapping3(apply, unapply, a1, a2, a3, uuid)
  }

  def mapping[R, A1, A2, A3, A4, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping](apply: Function4[A1, A2, A3, A4, R])(unapply: Function1[R, Option[(A1, A2, A3, A4)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}) = {
    ObjectMapping4(apply, unapply, a1, a2, a3, a4, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping](apply: Function5[A1, A2, A3, A4, A5, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}) = {
    ObjectMapping5(apply, unapply, a1, a2, a3, a4, a5, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping](apply: Function6[A1, A2, A3, A4, A5, A6, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}) = {
    ObjectMapping6(apply, unapply, a1, a2, a3, a4, a5, a6, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping](apply: Function7[A1, A2, A3, A4, A5, A6, A7, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}) = {
    ObjectMapping7(apply, unapply, a1, a2, a3, a4, a5, a6, a7, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping](apply: Function8[A1, A2, A3, A4, A5, A6, A7, A8, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}) = {
    ObjectMapping8(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping](apply: Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}) = {
    ObjectMapping9(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping](apply: Function10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}) = {
    ObjectMapping10(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping](apply: Function11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}) = {
    ObjectMapping11(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping](apply: Function12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}) = {
    ObjectMapping12(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping](apply: Function13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}) = {
    ObjectMapping13(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping](apply: Function14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}) = {
    ObjectMapping14(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping](apply: Function15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}) = {
    ObjectMapping15(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping](apply: Function16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}) = {
    ObjectMapping16(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping, M17 <: Mapping](apply: Function17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}, a17:M17{type T = A17}) = {
    ObjectMapping17(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, uuid)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1 <: Mapping, M2 <: Mapping, M3 <: Mapping, M4 <: Mapping, M5 <: Mapping, M6 <: Mapping, M7 <: Mapping, M8 <: Mapping, M9 <: Mapping, M10 <: Mapping, M11 <: Mapping, M12 <: Mapping, M13 <: Mapping, M14 <: Mapping, M15 <: Mapping, M16 <: Mapping, M17 <: Mapping, M18 <: Mapping](apply: Function18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]])(a1:M1{type T = A1}, a2:M2{type T = A2}, a3:M3{type T = A3}, a4:M4{type T = A4}, a5:M5{type T = A5}, a6:M6{type T = A6}, a7:M7{type T = A7}, a8:M8{type T = A8}, a9:M9{type T = A9}, a10:M10{type T = A10}, a11:M11{type T = A11}, a12:M12{type T = A12}, a13:M13{type T = A13}, a14:M14{type T = A14}, a15:M15{type T = A15}, a16:M16{type T = A16}, a17:M17{type T = A17}, a18:M18{type T = A18}) = {
    ObjectMapping18(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, uuid)
  }


  import Form._

  /**
   * Constructs a simple mapping for a text field.
   *
   * For example:
   * {{{
   * Form(text)
   * }}}
   */
  def text: FieldMapping[String] = of[String]()
  def text(constraints: Constraint[String]*): FieldMapping[String] = of[String](constraints = constraints:_*)

  /**
   * Constructs a simple mapping for required text field.
   *
   * Note that all field are always required to be present in the form unless
   * there are marked as optional explicitely. But a nonEmptyText defines text
   * field that must not be empty, even if present in the form.
   *
   * Example:
   * {{{
   * Form(nonEmptyText)
   * }}}
   */
  def nonEmptyText: FieldMapping[String] = text(Constraints.nonEmpty)

  /**
   * Constructs a simple mapping for a text field.
   *
   * For example:
   * {{{
   * Form(text(minLength=3))
   * }}}
   *
   * @param minLength minimum text length
   * @param maxLength maximum text length
   */
  def text(minLength: Int = 0, maxLength: Int = Int.MaxValue): FieldMapping[String] = (minLength, maxLength) match {
    case (0, Int.MaxValue) => text()
    case (min, Int.MaxValue) => text(Constraints.minLength(min))
    case (0, max) => text(Constraints.maxLength(max))
    case (min, max) => text(Constraints.minLength(min), Constraints.maxLength(max))
  }

  /**
   * Constructs a simple mapping for required text field.
   *
   * Example:
   * {{{
   * Form(nonEmptyText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  def nonEmptyText(minLength: Int = 0, maxLength: Int = Int.MaxValue): FieldMapping[String] = {
    val t = text(minLength, maxLength)
    t.copy(constraints = t.constraints :+ Constraints.nonEmpty)
  }

  /**
   * Constructs a simple mapping for a numeric field.
   *
   * For example:
   * {{{
   * Form(number)
   * }}}
   */
  def number: FieldMapping[Int] = of[Int]
  def number(contraints: Constraint[Int]*): FieldMapping[Int] = of[Int](constraints = contraints:_*)

  /**
   * Constructs a simple mapping for a numeric field (using a Long type behind).
   *
   * For example:
   * {{{
   * Form(longNumber)
   * }}}
   */
  def longNumber: FieldMapping[Long] = of[Long]
  def longNumber(constraints: Constraint[Long]*): FieldMapping[Long] = of[Long](constraints = constraints:_*)

  /**
   * Constructs a simple mapping for a numeric field.
   *
   * For example:
   * {{{
   * Form(number(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   */
  def number(min: Int = Int.MinValue, max: Int = Int.MaxValue): FieldMapping[Int] = (min, max) match {
    case (Int.MinValue, Int.MaxValue) => number
    case (min, Int.MaxValue) => number(Constraints.min(min))
    case (Int.MinValue, max) => number(Constraints.max(max))
    case (min, max) => number(Constraints.min(min), Constraints.max(max))
  }

  /**
   * Constructs a simple mapping for a numeric field (using a Long type behind).
   *
   * For example:
   * {{{
   * Form(longNumber(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   */
  def longNumber(min: Long = Long.MinValue, max: Long = Long.MaxValue): FieldMapping[Long] = (min, max) match {
    case (Long.MinValue, Long.MaxValue) => longNumber
    case (min, Long.MaxValue) => longNumber(Constraints.min(min))
    case (Long.MinValue, max) => longNumber(Constraints.max(max))
    case (min, max) => longNumber(Constraints.min(min), Constraints.max(max))
  }

  /**
   * Constructs a simple mapping for a date field.
   *
   * For example:
   * {{{
   * Form(date)
   * }}}
   */
  val date: FieldMapping[java.util.Date] = of[java.util.Date]

  /**
   * Define a fixed value in a mapping.
   * This mapping will not participate to the binding.
   *
   * @param value As we ignore this parameter in binding/unbinding we have to provide a default value.
   */
  def ignored[A](value: A): FieldMapping[A] = of(Formatter.ignoredFormat(value))

  /**
   * Defines an optional mapping.
   *
   * {{{
   * Form(
   *   optional(text)
   * )
   * }}}
   *
   * @param mapping The mapping to make optional.
   */
  def optional[A, M <: Mapping{type T = A}](mapping: M{type T = A}) = OptionalMapping[A, M](mapping)

  /**
   * Defines an default mapping, if the parameter is not present, provide a default value.
   *
   * {{{
   * Form(
   *   default(text, "The default text")
   * )
   * }}}
   *
   * @param mapping The mapping to make optional.
   * @param value The default value when mapping and the field is not present.
   */
  def default[A, M <: Mapping{type T = A}](mapping: M{type T = A}, value:A) = OptionalMapping[A, M](mapping).transform[A](_.getOrElse(value), Some(_))

  /**
   * Defines a repeated mapping.
   * {{{
   * Form(
   *   list(text)
   * )
   * }}}
   *
   * @param mapping The mapping to make repeated.
   */
  def list[A, M <: Mapping{type T = A}](mapping: M{type T = A}) = RepeatedMapping[A, M](mapping, uuid)

  /**
   * Defines a repeated mapping.
   * {{{
   * Form(
   *   seq(text)
   * )
   * }}}
   * 
   * @param mapping The mapping to make repeated.
   */
  def seq[A, M <: Mapping{type T = A}](mapping: M{type T = A}) = RepeatedMapping[A, M](mapping, uuid).transform[Seq[A]](_.toSeq, _.toList)

  /**
   * Constructs a simple mapping for a date field.
   *
   * For example:
   * {{{
   *   Form(date("dd-MM-yyyy"))
   * }}}
   *
   * @param pattern the date pattern, as defined in `java.text.SimpleDateFormat`
   */
  def date(pattern: String): FieldMapping[java.util.Date] = of[java.util.Date]

  /**
   * Constructs a simple mapping for an e-mail field.
   *
   * For example:
   * {{{
   *   Form(email)
   * }}}
   */
  def email: FieldMapping[String] = of[String](Constraints.pattern(
    """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r,
    "constraint.email",
    "error.email"))

  /**
   * Constructs a simple mapping for a Boolean field, such as a check-box.
   *
   * For example:
   * {{{
   *   Form(boolean)
   * }}}
   */
  def boolean: FieldMapping[Boolean] = of[Boolean]

  //TODO fix casting
  def checked(msg: String): FieldMapping[Boolean] = of[Boolean](Constraint((bool: Boolean) => if(bool) Valid else Invalid(msg) ))

  
}