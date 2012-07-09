

class Paypal{
	def addMoney(money:Double){}
	def removeMoney(money:Double){}
}

class Sparekonto{
	def innPenger(kroner:Int){}
	def utPenger(kroner:Int){}
}

class Account{
	def add(money: Double){}
	def withDraw(money: Double){}
}

trait Inn[F]{
	def	to(in:F, amount:Int):Unit
}

object Inn{
	implicit object PaypalIn extends Inn[Paypal]{
		override def to(in:Paypal, amount:Int) = in.addMoney(amount) 
	}
	implicit object AccountIn extends Inn[Account]{
		override def to(in:Account, amount:Int) = in.add(amount) 
	}
}
trait Ut[T]{
	def	from(in: T, amount:Int):Unit
}

object Ut{
	implicit object SparekontoIn extends Ut[Sparekonto]{
		override def from(in:Sparekonto, amount:Int) = in.utPenger(amount) 
	}
}

class Banking {
	
	def transfer[F: Ut, T: Inn](from:F, to:T, amount:Int){
		val fWrapper = implicitly[Ut[F]]
		val tWrapper = implicitly[Inn[T]]
		fWrapper.from(from, amount)
		tWrapper.to(to, amount)
	}
	
	val pp = new Paypal
	val sk = new Sparekonto
	
	transfer(sk, pp, 10)
	
	val ac = new Account
	
	transfer(sk, ac, 10)
	
}

