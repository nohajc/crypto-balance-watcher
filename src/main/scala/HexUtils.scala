object HexUtils {
  implicit class ArrayByteExt(arr: Array[Byte]) {
    def toHexString(prefix: String): String = prefix + arr.map(x => f"${x & 0xFF}%02x").mkString
    def toHexString: String = toHexString("")
  }
}
