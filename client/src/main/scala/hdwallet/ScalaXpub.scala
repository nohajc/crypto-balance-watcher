package hdwallet

import crypto._
import crypto.ECC.{Curve, Point}
import crypto.{HMAC, SHA512}

class ScalaPubKey(pubkey: Point) extends PubKey {
  override def compressed: Seq[Byte] = pubkey.getEncoded(compressed = true)
  override def uncompressed: Seq[Byte] = pubkey.getEncoded(compressed = false)
  def raw: Point = pubkey
}

class ScalaXpub(val xtype: String, pub: ScalaPubKey, chainCode: Seq[Byte]) extends Xpub {
  import ScalaXpub._
  override def publicKey: PubKey = pub

  override def deriveXpub(idx: Long): Xpub = {
    val pubKeyBytes = pub.raw.getEncoded(compressed = true).toArray
    val I = HMAC(SHA512)(chainCode.toArray, pubKeyBytes ++ intToBytesBE(idx.toInt))
    val IL = I.take(32)
    val IR = I.takeRight(32)

    val p = BigInt(1, IL)
    if (p >= curve.n) {
      return deriveXpub(idx + 1)
    }

    val Ki = curve.G.multiply(p).add(pub.raw)

    if (curve.isInfinity(Ki)) {
      return deriveXpub(idx + 1)
    }

    new ScalaXpub(xtype, new ScalaPubKey(Ki), IR)
  }
}

object ScalaXpub extends Xpub.Factory {
  lazy val curve: Curve = Curve.Secp256k1

  override def apply(xtype: String, depth: Int, fingerprint: Seq[Byte],
                     childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]): Xpub = {
    new ScalaXpub(xtype, new ScalaPubKey(Point.decodeFrom(curve, k)), c)
  }
}
