package crypto

object SHA256 extends SHA[Int] {
  override def blockSize: Int = 64
  def roundCount: Int = 64
  def msgLengthTypeSize: Int = 8

  override val HS = Array(
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19
  )

  override val KS = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  private val words = Array.fill[Int](roundCount)(0)

  override def iterate(regs: Array[Int], j: Int): Unit = {
    val S0 = Integer.rotateRight(regs(0), 2) ^
      Integer.rotateRight(regs(0), 13) ^
      Integer.rotateRight(regs(0), 22)

    val maj = (regs(0) & regs(1)) ^ (regs(0) & regs(2)) ^ (regs(1) & regs(2))
    val temp2 = S0 + maj

    val S1 = Integer.rotateRight(regs(4), 6) ^
      Integer.rotateRight(regs(4), 11) ^
      Integer.rotateRight(regs(4), 25)

    val ch = (regs(4) & regs(5)) ^ (~regs(4) & regs(6))
    val temp1 = regs(7) + S1 + ch + KS(j) + words(j)

    regs(7) = regs(6)
    regs(6) = regs(5)
    regs(5) = regs(4)
    regs(4) = regs(3) + temp1
    regs(3) = regs(2)
    regs(2) = regs(1)
    regs(1) = regs(0)
    regs(0) = temp1 + temp2
  }

  override def blockToWords(): Unit = {
    for (j <- 0 until 16) {
      words(j) = 0
      for (m <- 0 until 4) {
        words(j) |= ((block(j * 4 + m) & 0xFF) << (24 - m * 8))
      }
    }
  }

  override def fillRestOfWords(): Unit = {
    for (j <- 16 until roundCount) {
      val s0 = Integer.rotateRight(words(j - 15), 7) ^
        Integer.rotateRight(words(j - 15), 18) ^
        (words(j - 15) >>> 3)

      val s1 = Integer.rotateRight(words(j - 2), 17) ^
        Integer.rotateRight(words(j - 2), 19) ^
        (words(j - 2) >>> 10)

      words(j) = words(j - 16) + s0 + words(j - 7) + s1
    }
  }
}
