object ArrayBytes {
  implicit class Ext(arr: Array[Long]) {
    private val sizeofT = 8

    def bytes: Ext = this

    def toArray: Array[Byte] = {
      for (i <- 0 until arr.length * sizeofT) yield this(i)
    }.toArray

    def update(idx: Int, b: Byte): Unit = {
      val lidx = idx / sizeofT
      val shift = (idx % sizeofT) * 8L
      val oldVal = arr(lidx)
      val mask = 0xFFL << shift
      val newVal = (oldVal & ~mask) | ((b.toLong & 0xFF) << shift)
      arr(lidx) = newVal
    }

    def apply(idx: Int): Byte = {
      arr(idx / sizeofT) >>> ((idx % sizeofT) * 8L) & 0xFF
    }.toByte
  }
}
