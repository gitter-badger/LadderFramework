package org.ladderframework
import java.util.UUID
import java.security.SecureRandom
import java.util.Base64

object Utils {
	lazy val sr = SecureRandom.getInstance("SHA1PRNG")
	
	def uuid: String = {
		Base64.getEncoder().encodeToString(sr.generateSeed(16))
	}
	
	def uuidLong: String = {
		Base64.getEncoder().encodeToString(sr.generateSeed(64))
	}
}