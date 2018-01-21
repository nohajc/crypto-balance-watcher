package crypto

import utils.ArrayBytes._
import java.nio.ByteOrder

/*
  RIPEMD-160 hash implementation
  Ported from https://github.com/dlitz/pycrypto/blob/master/src/RIPEMD160.c
 */

object RIPEMD160 extends HashAlgo {

  override def blockSize: Int = 64
  private val digestSize = 20

  private val initialH = Array(0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)

  private val RL = Array(
    Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    Array(7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8),
    Array(3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12),
    Array(1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2),
    Array(4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13)
  )

  private val RR = Array(
    Array(5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12),
    Array(6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2),
    Array(15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13),
    Array(8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14),
    Array(12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11)
  )

  private val SL = Array(
    Array(11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8),
    Array(7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12),
    Array(11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5),
    Array(11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12),
    Array(9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6)
  )

  private val SR = Array(
    Array(8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6),
    Array(9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11),
    Array(9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5),
    Array(15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8),
    Array(8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11)
  )


  private val func = Array(
    (x: Int, y: Int, z: Int) => x ^ y ^ z,
    (x: Int, y: Int, z: Int) => (x & y) | (~x & z),
    (x: Int, y: Int, z: Int) => (x | ~y) ^ z,
    (x: Int, y: Int, z: Int) => (x & z) | (y & ~z),
    (x: Int, y: Int, z: Int) => x ^ (y | ~z)
  )

  private val KL = Array(
    0x00000000,
    0x5A827999,
    0x6ED9EBA1,
    0x8F1BBCDC,
    0xA953FD4E
  )

  private val KR = Array(
    0x50A28BE6,
    0x5C4DD124,
    0x6D703EF3,
    0x7A6D76E9,
    0x00000000
  )

  private def rol(s: Int, n: Int) = Integer.rotateLeft(n, s)


  private class State(val h: Array[Int] = initialH.clone,
                      var length: Long = 0,
                      val bufw: Array[Int] = Array.fill[Int](16)(0),
                      var bufpos: Int = 0) {
    override def clone: State = new State(h.clone, length, bufw.clone, bufpos)
  }

  private def bswap32(v: Int) =
    ((v & 0xFF) << 24) |
      ((v & 0xFF00) << 8) |
      ((v & 0xFF0000) >>> 8) |
      ((v & 0xFF000000) >>> 24)

  private def bswapDigest(d: Array[Int]): Unit = {
    for (i <- 0 until 4) {
      d(i) = bswap32(d(i))
    }
  }

  private def ripemd160Compress(self: State): Unit = {
    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) {
      bswapDigest(self.bufw)
    }

    var T = 0
    var AL, AR = self.h(0)
    var BL, BR = self.h(1)
    var CL, CR = self.h(2)
    var DL, DR = self.h(3)
    var EL, ER = self.h(4)

    for (round <- 0 until 5) {
      for (w <- 0 until 16) {
        T = rol(SL(round)(w),
          AL + func(round)(BL, CL, DL) +
            self.bufw(RL(round)(w)) + KL(round)) + EL
        AL = EL; EL = DL; DL = rol(10, CL); CL = BL; BL = T
      }

      for (w <- 0 until 16) {
        T = rol(SR(round)(w),
          AR + func(4 - round)(BR, CR, DR) +
            self.bufw(RR(round)(w)) + KR(round)) + ER
        AR = ER; ER = DR; DR = rol(10, CR); CR = BR; BR = T
      }
    }

    T = self.h(1) + CL + DR
    self.h(1) = self.h(2) + DL + ER
    self.h(2) = self.h(3) + EL + AR
    self.h(3) = self.h(4) + AL + BR
    self.h(4) = self.h(0) + BL + CR
    self.h(0) = T

    for (i <- self.bufw.indices) self.bufw(i) = 0
    self.bufpos = 0
  }

  private def arrayCopy(src: Array[Byte], srcPos: Int, dst: ArrayBytes[Int], dstPos: Int, length: Int): Unit = {
    var j = srcPos
    for (i <- dstPos until dstPos + length) {
      dst(i) = src(j)
      j += 1
    }
  }

  private def ripemd160Update(self: State, input: Array[Byte]): Unit = {
    var length = input.length
    var bytesNeeded = 0
    var inputIdx = 0

    while (length > 0) {
      bytesNeeded = 64 - self.bufpos

      if (length >= bytesNeeded) {
        arrayCopy(input, inputIdx, self.bufw.bytes, self.bufpos, bytesNeeded)
        self.bufpos += bytesNeeded
        self.length += bytesNeeded << 3
        inputIdx += bytesNeeded
        ripemd160Compress(self)
        length -= bytesNeeded
      }
      else {
        arrayCopy(input, inputIdx, self.bufw.bytes, self.bufpos, length)
        self.bufpos += length
        self.length += length << 3
        return
      }
    }
  }

  private def ripemd160Digest(self: State): Array[Byte] = {
    val tmp = self.clone

    tmp.bufw.bytes(tmp.bufpos) = 0x80.toByte
    tmp.bufpos += 1

    if (tmp.bufpos > 56) {
      tmp.bufpos = 64
      ripemd160Compress(tmp)
    }

    tmp.bufw(14) = (tmp.length & 0xFFFFFFFF).toInt
    tmp.bufw(15) = ((tmp.length >>> 32) & 0xFFFFFFFF).toInt
    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) {
      tmp.bufw(14) = bswap32(tmp.bufw(14))
      tmp.bufw(15) = bswap32(tmp.bufw(15))
    }
    tmp.bufpos = 64
    ripemd160Compress(tmp)

    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) {
      bswapDigest(tmp.h)
    }

    tmp.h.bytes.toArray
  }

  override def apply(data: Array[Byte]): Array[Byte] = {
    val state = new State
    ripemd160Update(state, data)
    ripemd160Digest(state)
  }
}
