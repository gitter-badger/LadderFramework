
package org.ladderframework.html.validation

import org.ladderframework.Context
import org.ladderframework.html.form.InputValueElement
import org.ladderframework.js.JsCmd

import JsClearValidationErrors.apply
import scalaz._
import scalaz.Failure.apply
import scalaz.Scalaz.{^ => ^}
import scalaz.Success.apply
import scalaz.ValidationNEL

trait FormValidation {

	type Input[U] = InputValueElement[U]
	type Key = String
	type Validation[U] = ValidationNEL[(Key, ValidationError), U]

	def map[U](i: Input[U])(toCmd: Validation[U] => JsCmd)(implicit param: Context#Params): JsCmd = {
		JsClearValidationErrors(i) & toCmd(validate(i))
	}

	def map[U1, U2](
		i1: Input[U1],
		i2: Input[U2])(toCmd: Validation[(U1, U2)] => JsCmd)(implicit param: Context#Params): JsCmd = {
		JsClearValidationErrors(i1, i2) & toCmd(^(validate(i1), validate(i2))(Tuple2.apply _)) 
	}

	def map[U1, U2, U3](
		i1: Input[U1],
		i2: Input[U2],
		i3: Input[U3])(toCmd: Validation[(U1, U2, U3)] => JsCmd)(implicit param: Context#Params): JsCmd = {
		JsClearValidationErrors(i1, i2, i3) & toCmd(^(validate(i1), validate(i2), validate(i3))(Tuple3.apply _))
	}

	def map[U1, U2, U3, U4](
		i1: Input[U1],
		i2: Input[U2],
		i3: Input[U3],
		i4: Input[U4])(toCmd: Validation[(U1, U2, U3, U4)] => JsCmd)(implicit param: Context#Params): JsCmd = {
		JsClearValidationErrors(i1, i2, i3, i4) & toCmd(^(validate(i1), validate(i2), validate(i3), validate(i4))(Tuple4.apply _))
	}

	def map[U1, U2, U3, U4, U5](
		i1: Input[U1],
		i2: Input[U2],
		i3: Input[U3],
		i4: Input[U4],
		i5: Input[U5])(toCmd: Validation[(U1, U2, U3, U4, U5)] => JsCmd)(implicit param: Context#Params): JsCmd = {
		JsClearValidationErrors(i1, i2, i3, i4, i5) & toCmd(^(validate(i1), validate(i2), validate(i3), validate(i4), validate(i5))(Tuple5.apply _))
	}

	def validate[U](input: Input[U])(implicit param: Context#Params): Validation[U] = {
		val value: Option[String] = input.getParamter(param)
		input.expectConvertValidate(value) match {
			case Success(s) => Success(s)
			case Failure(nelFailure) => Failure(nelFailure.map(f => input.validationId -> f))
		}
	}
	
}
