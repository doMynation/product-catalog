package inventory.util

import java.math.BigInteger
import java.security.MessageDigest

object StringOps {
  implicit def fromString(s: String) = StringOps(s)

  case class StringOps(a: String) {
    def md5: String =
      MessageDigest.getInstance("MD5")
        .digest(a.getBytes("UTF-8"))
        .map(0xFF & _)
        .map(s => "%02x".format(s))
        .mkString

    def sha256: String = {
      val o = MessageDigest.getInstance("SHA-256").digest(a.getBytes("UTF-8"))

      String.format("%032x", new BigInteger(1, o))
    }
  }

}
