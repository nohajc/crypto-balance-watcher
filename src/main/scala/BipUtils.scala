import java.nio.{ByteBuffer, ByteOrder}

import fr.acinq.bitcoin.{Base58, Base58Check, BinaryData, Crypto}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPublicKey, KeyPath, derivePublicKey}

object BipUtils {
  case class Xpub(xtype: String, depth: Int, fingerprint: Seq[Byte], childNumber: Seq[Byte], c: Seq[Byte], k: Seq[Byte])

  sealed trait AddressKind
  case object ReceivingAddr extends AddressKind
  case object ChangeAddr extends AddressKind

  trait AddressGenerator {
    def getAddressesFromXpubString(xpubStr: String, kind: AddressKind): Stream[String]
  }

  class DummyAddressGenerator extends AddressGenerator {
    def getAddressesFromXpubString(xpubStr: String, kind: AddressKind): Stream[String] = Stream.empty
  }

  class BTCAddressGenerator extends AddressGenerator {
    val AddrTypeP2PKH = 0
    val AddrTypeP2SH = 5

    val XpubHeaders = Map(
      "standard" -> 0x0488b21e,
      "p2wpkh-p2sh" -> 0x049d7cb2,
      "p2wsh-p2sh" -> 0x295b43f,
      "p2wpkh" -> 0x4b24746,
      "p2wsh" -> 0x2aa7ed3
    )

    def deserializeXpub(key: Seq[Byte]): Xpub = {
      val depth = key(4)
      val fingerprint = key.slice(5, 9)
      val childNumber = key.slice(9, 13)
      val c = key.slice(13, 13 + 32)
      val header = ByteBuffer.wrap(key.slice(0,4).toArray).getInt
      val found = XpubHeaders.find{case (_, v) => v == header}
      if (found.isEmpty) throw new RuntimeException("Invalid xpub format")
      val xtype = found.get._1
      val k = key.drop(13 + 32)

      Xpub(xtype, depth, fingerprint, childNumber, c, k)
    }

    def intToBinaryData(i: Int, len: Int): BinaryData =
      ByteBuffer.allocate(if (len < 4) 4 else len).order(ByteOrder.LITTLE_ENDIAN).putInt(i).array.take(len)

    def opPush(i: Int): BinaryData = {
      if (i < 0x4c)  intToBinaryData(i, 1)
      else if (i < 0xff) Seq(0x4c.toByte) ++ intToBinaryData(i, 1)
      else if (i < 0xffff) Seq(0x4d.toByte) ++ intToBinaryData(i, 2)
      else Seq(0x4e.toByte) ++ intToBinaryData(i, 4)
    }

    def pushScript(x: BinaryData): BinaryData = {
      opPush(x.size) ++ x
    }

    def hash160ToB58Address(h160: BinaryData, addrType: Int): String = {
      val s = Seq(addrType.toByte) ++ h160
      Base58.encode(s ++ Crypto.hash256(s).slice(0, 4))
    }


    def xpubToAddress(pubkey: Crypto.PublicKey, xtype: String): String = {
      require(pubkey.compressed)

      if (xtype == "p2wpkh-p2sh") {
        val scriptSig = Seq(0.toByte) ++ pushScript(pubkey.hash160)
        hash160ToB58Address(Crypto.hash160(scriptSig), AddrTypeP2SH)
      }
      else if (xtype == "standard") {
        Base58Check.encode(Base58.Prefix.PubkeyAddress, pubkey.hash160)
      }
      // TODO: support legacy addresses
      else {
        println(s"actual type: $xtype")
        throw new IllegalArgumentException
      }
    }

    def xpubFromString(xpubStr: String): (ExtendedPublicKey, String) = {
      val xpubBytes = Base58Check.decode(xpubStr) match {
        case (head, tail) => head +: tail
      }
      val xpubD = deserializeXpub(xpubBytes)
      (ExtendedPublicKey(xpubD.k, xpubD.c, 0, KeyPath(Seq(0)), 0), xpubD.xtype)
    }

    def getAddressesFromXpubString(xpubStr: String, kind: AddressKind): Stream[String] = {
      val kindIdx = kind match {
        case ReceivingAddr => 0L
        case ChangeAddr => 1L
      }
      val xpub = xpubFromString(xpubStr)
      val derived = derivePublicKey(xpub._1, kindIdx)

      def from(n: Int): Stream[String] = {
        val addr = xpubToAddress(derivePublicKey(derived, n).publicKey, xpub._2)
        addr #:: from(n + 1)
      }

      from(0)
    }
  }
  class BCHAddressGenerator extends BTCAddressGenerator

  class BTGAddressGenerator extends BTCAddressGenerator {
    override val AddrTypeP2PKH = 38
    override val AddrTypeP2SH = 23
  }

  class LTCAddressGenerator extends BTCAddressGenerator {
    //override val AddrTypeP2PKH = ???
    override val AddrTypeP2SH = 50

    override val XpubHeaders = Map(
      "p2wpkh-p2sh" -> 0x1b26ef6
      // TODO: other types
    )
  }

  class ETHAddressGenerator extends BTCAddressGenerator {
    override def xpubToAddress(pubkey: Crypto.PublicKey, xtype: String): String = {
      require(pubkey.compressed)

      if (xtype == "standard") {
        val uncompressed = pubkey.copy(compressed = false)
        s"0x${BinaryData(Keccak256(uncompressed.data.drop(1).toArray).takeRight(20)).toString}"
      }
      else {
        throw new IllegalArgumentException(s"actual type: $xtype")
      }
    }
  }

  class ETCAddressGenerator extends ETHAddressGenerator

  def convertToLTC3Address(addr: String): String = Base58Check.encode(5.toByte, Base58Check.decode(addr)._2)
}