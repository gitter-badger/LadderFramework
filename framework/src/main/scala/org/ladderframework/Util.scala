package org.ladderframework
import java.util.UUID
import java.security.SecureRandom
import java.util.Base64

object Utils {
	lazy val sr = SecureRandom.getInstance("SHA1PRNG")
	
	def uuid: String = UUID.randomUUID().toString
	
	def secureRandom: String = {
		val bytes = new Array[Byte](32)
 		sr.nextBytes(bytes)
		(Base64.getEncoder.encodeToString(bytes) + uuid).replaceAll("[\\/+=-]", "0")
	}
}