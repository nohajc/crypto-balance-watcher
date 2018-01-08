package hdwallet

abstract class PubKey {
  def compressed: Seq[Byte]
  def uncompressed: Seq[Byte]
}

abstract class Xpub {
  def xtype: String
  def publicKey: PubKey
  def deriveXpub(idx: Long): Xpub
}

object Xpub {
  abstract class Factory {
    def create(xtype: String, depth: Int, fingerprint: Seq[Byte],
               childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]): Xpub
  }

  private var factory: Factory = _

  def selectImplementation(f: Factory): Unit = {
    factory = f
  }

  def create(xtype: String, depth: Int, fingerprint: Seq[Byte],
             childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte]): Xpub = {
    factory.create(xtype, depth, fingerprint, childNumber, c, k)
  }
}
