package hdwallet

import fr.acinq.bitcoin.{Crypto, Protocol}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPublicKey, KeyPath, derivePublicKey}

class AcinqPubKey(pubkey: Crypto.PublicKey) extends PubKey {
  require(pubkey.compressed)
  private lazy val upub = pubkey.copy(compressed = false)
  override def compressed: Seq[Byte] = pubkey.data
  override def uncompressed: Seq[Byte] = upub.data
}

class AcinqXpub(val xtype: String, val xpub: ExtendedPublicKey) extends Xpub {
  private lazy val pub = new AcinqPubKey(xpub.publicKey)

  override def publicKey: PubKey = pub

  override def deriveXpub(idx: Long): Xpub = {
    val res = new AcinqXpub(xtype, derivePublicKey(xpub, idx))
    //Console.err.println(s"chaincode: ${xpub.chaincode}, key derivation: ${xpub.publicKey.toString} -> ${res.xpub.publicKey.toString}")
    res
  }
}

object AcinqXpub extends Xpub.Factory {
  override def apply(xtype: String, depth: Int, fingerprint: Seq[Byte],
                      childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]): Xpub = {
    new AcinqXpub(xtype, ExtendedPublicKey(k, c, depth, KeyPath(Seq(0)), 0))
  }
}
