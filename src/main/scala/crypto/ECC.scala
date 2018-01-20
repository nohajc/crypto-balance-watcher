package crypto

/*
 * Based on https://github.com/cryptocoinjs/ecurve
 */

object ECC {
  object Curve {
    lazy val Secp256k1: Curve = new Curve(
      BigInt("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f", 16),
      BigInt("00", 16),
      BigInt("07", 16),
      BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16),
      BigInt("01", 16),
      BigInt("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798", 16),
      BigInt("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16)
    )
  }

  class Curve(val p: BigInt,
              val a: BigInt,
              val b: BigInt,
              val n: BigInt,
              val h: BigInt,
              val Gx: BigInt,
              val Gy: BigInt) {
    val G: Point = Point.fromAffine(this, Gx, Gy)
    val infinity: Point = new Point(this, null, null, 0)
    private val pOverFour = (p + 1) >> 2
    val pLength: Int = (p.bitLength + 7) / 8

    def isInfinity(Q: Point): Boolean = Q.eq(infinity)

    def pointFromX(isOdd: Boolean, x: BigInt, compressed: Boolean = true): Point = {
      val alpha = (x.pow(3) + a * x + b).mod(p)
      val beta = alpha.modPow(pOverFour, p)

      val y =
        if (!beta.testBit(0) ^ !isOdd)
          p - beta
        else beta

      Point.fromAffine(this, x, y, compressed)
    }
  }

  final class Point(curve: Curve, val x: BigInt, val y: BigInt, val z: BigInt, compressed: Boolean = true) {
    val zInv: BigInt = if (z != BigInt(0)) z.modInverse(curve.p) else 1
    val affineX: BigInt = if (x != null) (x * zInv).mod(curve.p) else 2
    val affineY: BigInt = if (y != null) (y * zInv).mod(curve.p) else 3

    override def equals(that: scala.Any): Boolean = that match {
      case that: Point =>
        this.eq(that) ||
        curve.isInfinity(this) && curve.isInfinity(that) ||
        {
          val u = (that.y * z - y * that.z).mod(curve.p)
          u.signum == 0 &&
          {
            val v = (that.x * z - x * that.z).mod(curve.p)
            v.signum == 0
          }
        }
      case _ => false
    }

    def negate: Point = new Point(curve, x, curve.p - y, z)

    def add(b: Point): Point = {
      if (curve.isInfinity(this)) return b
      if (curve.isInfinity(b)) return this

      val x1 = x
      val y1 = y
      val x2 = b.x
      val y2 = b.y

      val u = (y2 * z - y1 * b.z).mod(curve.p)
      val v = (x2 * z - x1 * b.z).mod(curve.p)

      if (v.signum == 0) {
        if (u.signum == 0) {
          return this.twice
        }
        return curve.infinity
      }

      val v2 = v * v
      val v3 = v2 * v
      val x1v2 = x1 * v2
      val zu2 = u * u * z

      val x3 = (((zu2 - (x1v2 << 1)) * b.z - v3) * v).mod(curve.p)
      val y3 = ((x1v2 * 3 * u - y1 * v3 - zu2 * u) * b.z + u * v3).mod(curve.p)
      val z3 = (v3 * z * b.z).mod(curve.p)

      new Point(curve, x3, y3, z3)
    }

    def twice: Point = {
      if (curve.isInfinity(this)) return this
      if (y.signum == 0) return curve.infinity

      val x1 = x
      val y1 = y

      val y1z1 = (y1 * z).mod(curve.p)
      val y1sqz1 = (y1z1 * y1).mod(curve.p)
      val a = curve.a

      val w =
        if (a.signum == 0)
          (x1 * x1 * 3).mod(curve.p)
        else
          (x1 * x1 * 3 + z * z * a).mod(curve.p)

      val x3 = (((w * w - (x1 << 3) * y1sqz1) << 1) * y1z1).mod(curve.p)
      val y3 = (((w * 3 * x1 - (y1sqz1 << 1)) << 2) * y1sqz1 - w.pow(3)).mod(curve.p)
      val z3 = (y1z1.pow(3) << 3).mod(curve.p)

      new Point(curve, x3, y3, z3)
    }

    def multiply(k: BigInt): Point = {
      if (curve.isInfinity(this)) return this
      if (k.signum == 0) return curve.infinity

      val e = k
      val h = 3 * e
      val neg = this.negate
      var R = this

      for (i <- h.bitLength - 2 until 0 by -1) {
        val hBit = h.testBit(i)
        val eBit = e.testBit(i)

        R = R.twice

        if (hBit != eBit) {
          R = R.add(if (hBit) this else neg)
        }
      }

      R
    }

    def getEncoded(compressed: Boolean): Seq[Byte] = {
      if (curve.isInfinity(this)) return Array(0.toByte)

      val x = affineX
      val y = affineY
      val byteLength = curve.pLength

      val res =
        if (compressed) {
          (if (!y.testBit(0)) 0x02.toByte else 0x03.toByte) +: x.toByteArray.dropWhile(_ == 0)
        }
        else {
          0x04.toByte +: (x.toByteArray.dropWhile(_ == 0) ++ y.toByteArray.dropWhile(_ == 0))
        }

      res ++ Array.fill[Byte](byteLength - res.length)(0)
    }
  }

  object Point {
    def fromAffine(curve: Curve, x: BigInt, y: BigInt, compressed: Boolean = true): Point =
      new Point(curve, x, y, 1, compressed)

    def decodeFrom(curve: Curve, buffer: Seq[Byte]): Point = {
      val ty = buffer.head
      val compressed = ty != 0x04
      val byteLength = curve.pLength
      val x = BigInt(1, buffer.slice(1, 1 + byteLength).toArray)

      if (compressed) {
        require(buffer.lengthCompare(byteLength + 1) == 0)
        require(ty == 0x02 || ty == 0x03)

        val isOdd = ty == 0x03
        curve.pointFromX(isOdd, x, compressed)
      }
      else {
        require(buffer.lengthCompare(byteLength + byteLength + 1) == 0)

        val y = BigInt(1, buffer.takeRight(byteLength).toArray)
        Point.fromAffine(curve, x, y, compressed)
      }
    }
  }
}
