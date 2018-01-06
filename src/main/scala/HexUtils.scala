object HexUtils {
  implicit class ArrayByteExt(arr: Array[Byte]) {
    def toHexString(prefix: String): String = prefix + arr.map("%02x".format(_)).mkString
    def toHexString: String = toHexString("")
  }
}
