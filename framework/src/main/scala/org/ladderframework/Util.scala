package org.ladderframework
import java.util.UUID
import java.security.SecureRandom
import java.util.Base64

object Utils {
	lazy val sr = SecureRandom.getInstance("SHA1PRNG")
	
	def uuid: String = UUID.randomUUID().toString
	
	def secureRandom: String = {
		(Base64.getEncoder().encodeToString(sr.generateSeed(32)) + uuid).replaceAll("[\\/+=-]", "0")
	}
}