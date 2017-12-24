package ds

private[ds] final class DictHT[A, B] (val initialSize: Int = 0) {
  private var table = new Array[DictEntry[A, B]](initialSize)

  var size: Int = initialSize

  var sizeMask: Int = if (initialSize == 0) initialSize else initialSize - 1

  var used: Int = 0

  def get(idx: Int): DictEntry[A, B] = table(idx)

  def set(idx: Int, he: DictEntry[A, B]): Unit = {
    table(idx) = he
    used += 1
  }

  def clear = table = new Array[DictEntry[A, B]](initialSize)
}
