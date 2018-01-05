/*
  Port of tiny_sha3 to Scala
  Original code: https://github.com/mjosaarinen/tiny_sha3

  Modified to compute the old version of SHA3 used in Ethereum


  The MIT License (MIT)

  Copyright (c) 2015 Markku-Juhani O. Saarinen

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

 */

object Keccak {
  import ArrayBytes._

  private class Sha3Ctx(var st: Array[Long], var pt: Int, val rsiz: Int, val mdlen: Int)

  private val KeccakfRounds: Int = 24

  private val KeccakfRndc = Array[Long](
    0x0000000000000001L, 0x0000000000008082L, 0x800000000000808aL,
    0x8000000080008000L, 0x000000000000808bL, 0x0000000080000001L,
    0x8000000080008081L, 0x8000000000008009L, 0x000000000000008aL,
    0x0000000000000088L, 0x0000000080008009L, 0x000000008000000aL,
    0x000000008000808bL, 0x800000000000008bL, 0x8000000000008089L,
    0x8000000000008003L, 0x8000000000008002L, 0x8000000000000080L,
    0x000000000000800aL, 0x800000008000000aL, 0x8000000080008081L,
    0x8000000000008080L, 0x0000000080000001L, 0x8000000080008008L
  )

  private val KeccakfRotc = Array[Int](
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8, 25, 43, 62, 18, 39, 61, 20, 44
  )

  private val KeccakfPiln = Array[Int](
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1
  )

  private def rotl64(x: Long, y: Int) = (x << y) | (x >>> (64 - y))

  private def sha3Keccakf(st: Array[Long]): Unit = {
    var t: Long = 0
    val bc = Array.fill[Long](5)(0)

    for (r <- 0 until KeccakfRounds) {
      for (i <- 0 until 5) {
        bc(i) = st(i) ^ st(i + 5) ^
          st(i + 10) ^ st(i + 15) ^ st(i + 20)
      }

      for (i <- 0 until 5) {
        t = bc((i + 4) %  5) ^ rotl64(bc((i + 1) % 5), 1)
        for (j <- 0 until 25 by 5) {
          st(j + i) ^= t
        }
      }

      t = st(1)
      for (i <- 0 until 24) {
        val j = KeccakfPiln(i)
        bc(0) = st(j)
        st(j) = rotl64(t, KeccakfRotc(i))
        t = bc(0)
      }

      for (j <- 0 until 25 by 5) {
        for (i <- 0 until 5) {
          bc(i) = st(j + i)
        }
        for (i <- 0 until 5) {
          st(j + i) ^= ((~bc((i + 1) % 5)) & bc((i + 2) % 5))
        }
      }

      st(0) ^= KeccakfRndc(r)
    }
  }

  private def sha3Init(mdlen: Int): Sha3Ctx = {
    val st = Array.fill[Long](25)(0)
    new Sha3Ctx(st, 0, 200 - 2 * mdlen, mdlen)
  }

  private def sha3Update(c: Sha3Ctx, data: Array[Byte]): Unit = {
    var j = c.pt
    val b = c.st.bytes

    for (i <- data.indices) {
      val x = b(j)
      b(j) = (b(j) ^ data(i)).toByte
      j += 1
      if (j >= c.rsiz) {
        sha3Keccakf(c.st)
        j = 0
      }
    }
    c.pt = j
  }

  private def sha3Final(c: Sha3Ctx): Array[Byte] = {
    val b = c.st.bytes
    b(c.pt) = (b(c.pt) ^ 0x01).toByte
    b(c.rsiz - 1) = (b(c.rsiz - 1) ^ 0x80).toByte
    sha3Keccakf(c.st)

    b.toArray.take(c.mdlen)
  }

  def sha3(in: Array[Byte], mdlen: Int): Array[Byte] = {
    val context = sha3Init(mdlen)
    sha3Update(context, in)
    sha3Final(context)
  }
}

object Keccak256 {
  def apply(in: Array[Byte]): Array[Byte] = Keccak.sha3(in, 32)
}
