package org.ladderframework.html.validation

import scalaz._
import Scalaz._
import org.ladderframework.Context

trait InputValidation[T] {
	val validationId:String = Context.createUUID
	def expectConvertValidate: Option[String] => ValidationNEL[ValidationError, T]
}

object ExpectConvertValidate{
	
	type ValidationFunc[T] = String => ValidationNEL[ValidationError, T]
	
	def optional[T] = new OptionalWrapper[T]
	def mandatory[T] = new MandatoryWrapper[T]
	
	def convert[T](c: String => ValidationNEL[ValidationError, T] = asString): String => ValidationNEL[ValidationError, T] = c
	
	def validate[T](validations: Validation[T] *) = {
		Validate(validations:_*)
	}
	
	implicit def optional2validation(opt: OptionalWrapper[String]): Option[String] => ValidationNEL[ValidationError, Option[String]] = {
		opt =>> convert(asString) =>> validate()
	}
	
	implicit def optionalConverter2validation[T](ocw: OptionalConvertWrapper[T]): Option[String] => ValidationNEL[ValidationError, Option[T]] = {
		ocw =>> validate()
	}
	
	implicit def mandatory2validation(man : MandatoryWrapper[String]): Option[String] => ValidationNEL[ValidationError, String] = {
		man =>> convert(asString) =>> validate()
	}
	
	implicit def mandatoryConverter2validation[T](mcw:MandatoryConvertWrapper[T]): Option[String] => ValidationNEL[ValidationError, T] = {
		mcw =>> validate()
	}
}
	

class OptionalWrapper[T]{
	def =>>(convert: String => ValidationNEL[ValidationError, T]): OptionalConvertWrapper[T] = {
		new OptionalConvertWrapper[T](convert) 
	}
	
	def =>>(validate:Validate[String]): Option[String] => ValidationNEL[ValidationError, Option[String]] = {
		new OptionalConvertWrapper[String](asString) =>> validate 
	}
	
}

class OptionalConvertWrapper[T](convert: String => ValidationNEL[ValidationError, T]){
	def =>>(validate:Validate[T]): Option[String] => ValidationNEL[ValidationError, Option[T]] = in => {
		in match {
			case None => None.success
			case Some(in) => convert(in).flatMap(validate.validate) match {
				case Success(s) => Success(Option(s))
				case Failure(f) => f.failure
			}
		}
	}
}

class MandatoryWrapper[T]{
	def =>>(convert: String => ValidationNEL[ValidationError, T]): MandatoryConvertWrapper[T] = {
		new MandatoryConvertWrapper[T](convert) 
	}
}

class MandatoryConvertWrapper[T](convert: String => ValidationNEL[ValidationError, T]){
	def =>>(validate:Validate[T]): Option[String] => ValidationNEL[ValidationError, T] = in => {
		mandatory(in).flatMap(convert).flatMap(validate.validate)
	} 
		
	def mandatory(in: Option[String]): ValidationNEL[ValidationError, String] = {
		in match { 	
			case None => ValidationError("missing value").failureNel
			case Some(s) if s.isEmpty => ValidationError("missing value").failureNel
			case Some(s) => s.success
		}
	}
}

object asString extends (String => ValidationNEL[ValidationError, String]){
	override def apply(in: String): ValidationNEL[ValidationError, String] = in.success
}

object toInt extends (String => ValidationNEL[ValidationError, Int]){
	val Numeric = "(-?\\d+)".r
	override def apply(in: String): ValidationNEL[ValidationError, Int] = {
		in match {
			case Numeric(i) => i.toInt.success
			case other => ValidationError(other + " is not an int").failureNel
		}
	}
}

case class Validate[T](validations: Validation[T]*) {
	def validate(in: T) : ValidationNEL[ValidationError, T] = {
		val failures = validations.map(_.validate(in)).filter(_.isFailure)
		failures.fold(in.successNel)((a, b) => {
			(a, b) match {
				case (Success(s), Failure(f)) => f.failure
				case (Failure(f1), Failure(f2)) => f1.append(f2).failure
				case (Failure(f1), Success(f2)) => f1.failure
				case (Success(s2), Success(_)) => s2.successNel
			}
			/*(a , b) match {
			case (Failure(f1), Failure(f2)) => (f1 :::> (f2.list)).failure
			case _ =>*/
		})
	}
}

trait Validation[T] {
	def validate(in: T) : ValidationNEL[ValidationError, T]
}

case class Min(min: Int) extends Validation[Int]{ 
	override def validate(in: Int) : ValidationNEL[ValidationError, Int] = {
		if (in >= min){
			in.success
		}else {
			ValidationError(in + " is to low").failureNel
		}
	}
}

