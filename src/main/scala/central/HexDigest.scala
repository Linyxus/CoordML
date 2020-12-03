package central

import java.security.MessageDigest

// create hex digest of any object
class HexDigest(val algorithm: String = "SHA-256") {
  val digester: MessageDigest = MessageDigest.getInstance(algorithm)

  def byteArrayToHex(buf: Array[Byte]): String = buf.map { "%02x" format _ } mkString ""

  def of(x: Any): String = byteArrayToHex(digester.digest(x.toString.getBytes))
}

/**
 * Example usage:
 *  HexDigest().of("123")
 */
object HexDigest {
  def apply(algorithm: String = "SHA-256"): HexDigest = new HexDigest(algorithm)
}
