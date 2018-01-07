package crypto

trait SHAWord[T] {
  def toBytesBE(v: T): Array[Byte]
  def add(a: T, b: T): T
}

abstract class SHA[T: SHAWord] extends HashAlgo {
  def blockSize: Int
  def roundCount: Int
  def msgLengthTypeSize: Int

  protected val HS: Array[T]
  protected val KS: Array[T]
  protected val block: Array[Byte] = Array.fill[Byte](blockSize)(0)

  protected def iterate(regs: Array[T], j: Int): Unit
  protected def blockToWords(): Unit
  protected def fillRestOfWords(): Unit

  private val shaWord = implicitly[SHAWord[T]]

  protected def wordToBytes(w: T): Array[Byte] = shaWord.toBytesBE(w)

  protected def padLeft(data: Array[Byte], length: Int): Array[Byte] = {
    Array.fill[Byte](length - data.length)(0) ++ data
  }

  protected def padMessage(data: Array[Byte]): Array[Byte] = {
    val length = data.length
    val tail = length % blockSize
    val lenInBytes = padLeft(longToBytesBE(length * 8), msgLengthTypeSize)
    val npad = (
      if (tail < blockSize - msgLengthTypeSize) 0
      else blockSize
      ) + blockSize - msgLengthTypeSize - 1 - tail

    data ++ Array(0x80.toByte) ++ Array.fill[Byte](npad)(0) ++ lenInBytes
  }

  override def apply(data: Array[Byte]): Array[Byte] = {
    val padded = padMessage(data)
    val hs = HS.clone

    for (i <- padded.indices by blockSize) {
      val regs = hs.clone
      Array.copy(padded, i, block, 0, blockSize)

      blockToWords()
      fillRestOfWords()

      for (j <- 0 until roundCount) {
        iterate(regs, j)
      }

      for (j <- 0 until 8) {
        hs(j) = shaWord.add(hs(j), regs(j))
      }
    }

    hs.flatMap(wordToBytes)
  }
}
