trait HashAlgo {
  def blockSize: Int
  def apply(data: Array[Byte]): Array[Byte]
}
