package org.ladderframework{

	package object validation{
		
		trait Validation[T]{
			def validate(t:T):List[ValidationError] = Nil
		}
		
		implicit def unit2ValidationErrorList(unit:Unit):List[ValidationError] = Nil 
		implicit def validationError2List(validationError:ValidationError):List[ValidationError] = validationError :: Nil 
	}
	
	package validation{
		case class ValidationError(val msg:String){
			def ::(validationError:ValidationError):List[ValidationError] = {
				this :: validationError :: Nil
			}
		}
	}
}