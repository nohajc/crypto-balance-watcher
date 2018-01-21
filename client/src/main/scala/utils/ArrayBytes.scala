package utils

object ArrayBytes {

  trait NumericElem[T] {
    def fromT(v: T): Long
    def toT(l: Long): T
    def size: Int
  }

  implicit val arrayElemLong: NumericElem[Long] = new NumericElem[Long] {
    override def fromT(v: Long): Long = v
    override def toT(l: Long): Long = l
    override def size: Int = 8
  }

  implicit val arrayElemInt: NumericElem[Int] = new NumericElem[Int] {
    override def fromT(v: Int): Long = v.toLong & 0xFFFFFFFF
    override def toT(l: Long): Int = l.toInt
    override def size: Int = 4
  }

  implicit class Ext[T: NumericElem](arr: Array[T]) {
    private val arrayElem = implicitly[NumericElem[T]]
    private val sizeofT = arrayElem.size

    def bytes: Ext[T] = this

    def toArray: Array[Byte] = {
      for (i <- 0 until arr.length * sizeofT) yield this(i)
    }.toArray

    def update(idx: Int, b: Byte): Unit = {
      val lidx = idx / sizeofT
      val shift = (idx % sizeofT) * 8L
      val oldVal = arrayElem.fromT(arr(lidx))
      val mask = 0xFFL << shift
      val newVal = (oldVal & ~mask) | ((b.toLong & 0xFF) << shift)
      arr(lidx) = arrayElem.toT(newVal)
    }

    def apply(idx: Int): Byte = {
      (arrayElem.fromT(arr(idx / sizeofT)) >>> ((idx % sizeofT) * 8L)) & 0xFF
    }.toByte
  }

  type ArrayBytes[T] = Ext[T]
}
