import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import fr.acinq.bitcoin.BinaryData

object HMAC {
  def apply(secret: String, msg: String): String = {
    val keySpec = new SecretKeySpec(secret.getBytes, "SHA256")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(keySpec)
    val hash = mac.doFinal(msg.getBytes)
    BinaryData(hash).toString
  }
}
