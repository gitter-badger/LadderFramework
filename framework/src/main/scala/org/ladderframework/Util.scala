package org.ladderframework
import java.util.UUID
import java.security.SecureRandom
import akka.parboiled2.util.Base64

object Utils {
	lazy val sr = SecureRandom.getInstance("SHA1PRNG")
	
	def uuid: String = UUID.randomUUID().toString
	
	def secureRandom: String = {
		val bytes = new Array[Byte](32)
 		sr.nextBytes(bytes)
		(Base64.rfc2045().encodeToString(bytes, false) + uuid).replaceAll("[\\/+=-]", "0")
	}
}