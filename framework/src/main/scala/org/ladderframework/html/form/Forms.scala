package org.ladderframework.html.form

import org.ladderframework.html.form.Formats._

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
object Forms {

	implicit class TransformWrapper[A, M <: Mapping[M]{type T = A}](mapping: M{type T = A}){
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
  def of[T](implicit binder: Formatter[T]): FieldMapping[T] = FieldMapping[T]()(binder)

  /**
   * Creates a Mapping of type `T`.
   *
   * For example:
   * {{{
   * Form(of[String]("email")
   * }}}
   *
   * @tparam T the mapping type
   * @return a mapping for a simple field
   */
  def of[T](key: String)(implicit binder: Formatter[T]): FieldMapping[T] = FieldMapping[T](key)(binder)
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
  def mapping[R, A1, M1 <: Mapping[M1]{type T = A1}](apply: Function1[A1, R])(unapply: Function1[R, Option[(A1)]])(a1: M1) = {
    ObjectMapping1(apply, unapply, a1)
  }

  def mapping[R, A1, A2, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}](apply: Function2[A1, A2, R])(unapply: Function1[R, Option[(A1, A2)]])(a1: M1, a2: M2) = {
    ObjectMapping2(apply, unapply, a1, a2)
  }

  def mapping[R, A1, A2, A3, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}](apply: Function3[A1, A2, A3, R])(unapply: Function1[R, Option[(A1, A2, A3)]])(a1: M1, a2: M2, a3: M3) = {
    ObjectMapping3(apply, unapply, a1, a2, a3)
  }

  def mapping[R, A1, A2, A3, A4, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}](apply: Function4[A1, A2, A3, A4, R])(unapply: Function1[R, Option[(A1, A2, A3, A4)]])(a1: M1, a2: M2, a3: M3, a4: M4) = {
    ObjectMapping4(apply, unapply, a1, a2, a3, a4)
  }

  def mapping[R, A1, A2, A3, A4, A5, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}](apply: Function5[A1, A2, A3, A4, A5, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5) = {
    ObjectMapping5(apply, unapply, a1, a2, a3, a4, a5)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}](apply: Function6[A1, A2, A3, A4, A5, A6, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6) = {
    ObjectMapping6(apply, unapply, a1, a2, a3, a4, a5, a6)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}](apply: Function7[A1, A2, A3, A4, A5, A6, A7, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7) = {
    ObjectMapping7(apply, unapply, a1, a2, a3, a4, a5, a6, a7)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}](apply: Function8[A1, A2, A3, A4, A5, A6, A7, A8, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8) = {
    ObjectMapping8(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}](apply: Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9) = {
    ObjectMapping9(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}](apply: Function10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10) = {
    ObjectMapping10(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}](apply: Function11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11) = {
    ObjectMapping11(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}](apply: Function12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12) = {
    ObjectMapping12(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}](apply: Function13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13) = {
    ObjectMapping13(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}](apply: Function14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13, a14: M14) = {
    ObjectMapping14(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}](apply: Function15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13, a14: M14, a15: M15) = {
    ObjectMapping15(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}](apply: Function16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13, a14: M14, a15: M15, a16: M16) = {
    ObjectMapping16(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}, M17 <: Mapping[M17]{type T = A17}](apply: Function17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13, a14: M14, a15: M15, a16: M16, a17: M17) = {
    ObjectMapping17(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
  }

  def mapping[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}, M17 <: Mapping[M17]{type T = A17}, M18 <: Mapping[M18]{type T = A18}](apply: Function18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R])(unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]])(a1: M1, a2: M2, a3: M3, a4: M4, a5: M5, a6: M6, a7: M7, a8: M8, a9: M9, a10: M10, a11: M11, a12: M12, a13: M13, a14: M14, a15: M15, a16: M16, a17: M17, a18: M18) = {
    ObjectMapping18(apply, unapply, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
  }


  import Form._
import Formats._

  /**
   * Constructs a simple mapping for a text field.
   *
   * For example:
   * {{{
   * Form(text)
   * }}}
   */
  def text: FieldMapping[String] = of[String]

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
  def nonEmptyText: FieldMapping[String] = text verifying Constraints.nonEmpty

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
    case (0, Int.MaxValue) => text
    case (min, Int.MaxValue) => text verifying Constraints.minLength(min)
    case (0, max) => text verifying Constraints.maxLength(max)
    case (min, max) => text verifying (Constraints.minLength(min), Constraints.maxLength(max))
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
  def nonEmptyText(minLength: Int = 0, maxLength: Int = Int.MaxValue): FieldMapping[String] = text(minLength, maxLength) verifying Constraints.nonEmpty

  /**
   * Constructs a simple mapping for a numeric field.
   *
   * For example:
   * {{{
   * Form(number)
   * }}}
   */
  def number: FieldMapping[Int] = of[Int]

  /**
   * Constructs a simple mapping for a numeric field (using a Long type behind).
   *
   * For example:
   * {{{
   * Form(longNumber)
   * }}}
   */
  def longNumber: FieldMapping[Long] = of[Long]

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
    case (min, Int.MaxValue) => number verifying Constraints.min(min)
    case (Int.MinValue, max) => number verifying Constraints.max(max)
    case (min, max) => number verifying (Constraints.min(min), Constraints.max(max))
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
    case (min, Long.MaxValue) => longNumber verifying Constraints.min(min)
    case (Long.MinValue, max) => longNumber verifying Constraints.max(max)
    case (min, max) => longNumber verifying (Constraints.min(min), Constraints.max(max))
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
  def ignored[A](value: A): FieldMapping[A] = of(ignoredFormat(value))

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
  def optional[OT, M <: Mapping[M]{type T = OT}](mapping: M{type T = OT}): OptionalMapping[OT, M] = OptionalMapping(mapping)

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
  def default[A, M <: Mapping[M]{type T = A}](mapping: M{type T = A}, value:A) = OptionalMapping[A, M](mapping).transform[A](_.getOrElse(value), Some(_))

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
  def list[A, M <: Mapping[M]{type T = A}](mapping: M{type T = A}) = RepeatedMapping[A, M](mapping)
  def list[A, M <: Mapping[M]{type T = A}](mapping: M{type T = A}, key: String) = RepeatedMapping[A, M](mapping, key)

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
  def seq[A, M <: Mapping[M]{type T = A}](mapping: M{type T = A}) = RepeatedMapping[A, M](mapping).transform[Seq[A]](_.toSeq, _.toList)

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
  def date(pattern: String): FieldMapping[java.util.Date] = of[java.util.Date] as dateFormat(pattern)

  /**
   * Constructs a simple mapping for a date field (mapped as `sql.Date type`).
   *
   * For example:
   * {{{
   *   Form(sqlDate)
   * }}}
   */
  val sqlDate: FieldMapping[java.sql.Date] = of[java.sql.Date]

  /**
   * Constructs a simple mapping for a date field (mapped as `sql.Date type`).
   *
   * For example:
   * {{{
   *   Form(sqlDate("dd-MM-yyyy"))
   * }}}
   *
   * @param pattern the date pattern, as defined in `java.text.SimpleDateFormat`
   */
  def sqlDate(pattern: String): FieldMapping[java.sql.Date] = of[java.sql.Date] as sqlDateFormat(pattern)

//  /**
//   * Constructs a simple mapping for a date field (mapped as `org.joda.time.DateTime type`).
//   *
//   * For example:
//   * {{{
//   *   Form("birthdate" -> jodaDate)
//   * }}}
//   */
//  val jodaDate: Mapping[org.joda.time.DateTime] = of[org.joda.time.DateTime]
//
//  /**
//   * Constructs a simple mapping for a date field (mapped as `org.joda.time.DateTime type`).
//   *
//   * For example:
//   * {{{
//   *   Form("birthdate" -> jodaDate("dd-MM-yyyy"))
//   * }}}
//   *
//   * @param pattern the date pattern, as defined in `org.joda.time.format.DateTimeFormat`
//   */
//  def jodaDate(pattern: String): Mapping[org.joda.time.DateTime] = of[org.joda.time.DateTime] as jodaDateTimeFormat(pattern)
//
//  /**
//   * Constructs a simple mapping for a date field (mapped as `org.joda.time.LocalDatetype`).
//   *
//   * For example:
//   * {{{
//   * Form("birthdate" -> jodaLocalDate)
//   * }}}
//   */
//  val jodaLocalDate: Mapping[org.joda.time.LocalDate] = of[org.joda.time.LocalDate]
//
//  /**
//   * Constructs a simple mapping for a date field (mapped as `org.joda.time.LocalDate type`).
//   *
//   * For example:
//   * {{{
//   * Form("birthdate" -> jodaLocalDate("dd-MM-yyyy"))
//   * }}}
//   *
//   * @param pattern the date pattern, as defined in `org.joda.time.format.DateTimeFormat`
//   */
//  def jodaLocalDate(pattern: String): Mapping[org.joda.time.LocalDate] = of[org.joda.time.LocalDate] as jodaLocalDateFormat(pattern)

  /**
   * Constructs a simple mapping for an e-mail field.
   *
   * For example:
   * {{{
   *   Form(email)
   * }}}
   */
  def email: FieldMapping[String] = of[String] verifying Constraints.pattern(
    """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r,
    "constraint.email",
    "error.email")

  /**
   * Constructs a simple mapping for a Boolean field, such as a check-box.
   *
   * For example:
   * {{{
   *   Form(boolean)
   * }}}
   */
  def boolean: FieldMapping[Boolean] = of[Boolean]
  def boolean(key: String): FieldMapping[Boolean] = of[Boolean](key)

  //TODO fix casting
  def checked(msg: String): FieldMapping[Boolean] = (boolean verifying (msg, (bool: Boolean) => bool)).asInstanceOf[FieldMapping[Boolean]]

  
}