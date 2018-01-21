package object crypto {
  def intToBytesBE(i: Int): Array[Byte] = { // BigEndian
    for (c <- 0 until 4) yield (i >>> (24 - 8 * c)).toByte
  }.toArray

  def longToBytesBE(l: Long): Array[Byte] = { // BigEndian
    for (c <- 0 until 8) yield (l >>> (56 - 8 * c)).toByte
  }.toArray

  implicit val longWord = new SHAWord[Long] {
    override def toBytesBE(v: Long): Array[Byte] = longToBytesBE(v)
    override def add(a: Long, b: Long): Long = a + b
  }

  implicit val intWord = new SHAWord[Int] {
    override def toBytesBE(v: Int): Array[Byte] = intToBytesBE(v)
    override def add(a: Int, b: Int): Int = a + b
  }
}
