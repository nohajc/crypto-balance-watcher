object Base58 {
  private val charset = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val charval = charset.zipWithIndex.toMap

  def encode(data: Seq[Byte]): String = {
    if (data.nonEmpty) {
      val num = BigInt(1, data.toArray)

      def convert(num: BigInt): Stream[Char] = {
        if (num == 0) Stream.empty
        else {
          val (x, r) = num /% 58
          charset(r.intValue) #:: convert(x)
        }
      }

      (data.takeWhile(_ == 0).map(_ => charset(0)) ++ convert(num).reverse).mkString
    }
    else ""
  }

  def decode(data: String): Seq[Byte] = {
    var leadingZeros = 0
    val bnum = data.foldLeft(BigInt(0)) { case (num, c) =>
        val v = charval(c)
        if (num == 0 && v == 0) leadingZeros += 1
        num * 58 + v
    }
    Array.fill[Byte](leadingZeros)(0) ++ bnum.toByteArray.dropWhile(_ == 0)
  }
}

object Base58Check {
  def checksum(data: Seq[Byte]): Seq[Byte] = SHA256(SHA256(data.toArray)).take(4)

  def encode(prefix: Byte, data: Seq[Byte]): String = {
    val str = prefix +: data
    val chk = checksum(str)
    Base58.encode(str ++ chk)
  }

  def decode(data: String): Seq[Byte] = {
    val strchk = Base58.decode(data)
    val chk = strchk.takeRight(4)
    val str = strchk.dropRight(4)
    require(checksum(str) == chk, "invalid checksum in Base58Check encoded data")
    str
  }
}
