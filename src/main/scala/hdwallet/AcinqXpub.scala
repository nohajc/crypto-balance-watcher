package hdwallet

import fr.acinq.bitcoin.Crypto
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPublicKey, KeyPath, derivePublicKey}

class AcinqPubKey(pubkey: Crypto.PublicKey) extends PubKey {
  require(pubkey.compressed)
  private lazy val upub = pubkey.copy(compressed = false)
  override def compressed: Seq[Byte] = pubkey.data
  override def uncompressed: Seq[Byte] = upub.data
}

class AcinqXpub(val xtype: String, xpub: ExtendedPublicKey) extends Xpub {
  private lazy val pub = new AcinqPubKey(xpub.publicKey)

  def this(xtype: String, depth: Int, fingerprint: Seq[Byte], childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]) = {
    this(xtype, ExtendedPublicKey(k, c, depth, KeyPath(Seq(0)), 0))
  }

  override def publicKey: PubKey = pub

  override def deriveXpub(idx: Long): Xpub = {
    new AcinqXpub(xtype, derivePublicKey(xpub, idx))
  }
}

class AcinqXpubFactory extends Xpub.Factory {
  override def create(xtype: String, depth: Int, fingerprint: Seq[Byte],
                      childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]): Xpub = {
    new AcinqXpub(xtype, ExtendedPublicKey(k, c, depth, KeyPath(Seq(0)), 0))
  }
}
