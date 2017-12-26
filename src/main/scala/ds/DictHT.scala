package ds

/*private[ds]*/ final class DictHT[A, B] (val initialSize: Int = 0) {
  private val table = new Array[DictEntry[A, B]](initialSize)

  var size: Int = initialSize

  var sizeMask: Int = if (initialSize == 0) initialSize else initialSize - 1

  var used: Int = 0

  def get(idx: Int): DictEntry[A, B] = {
    table(idx)
  }

  def put(idx: Int, he: DictEntry[A, B]): Unit = {
    table(idx) = he
    used += 1
  }

  def clear(): Unit = {
    for (i <- size - 1 to 0 by -1)
      table(i) = null.asInstanceOf[DictEntry[A, B]]

    used = 0
  }

  def filterIndex(hash: Int, key: A): Option[Int] = {
    val idx = hash & sizeMask
    val he = get(idx)

    if (he == null) Some(idx)
    else {
      he.filter((e: DictEntry[A, B]) => e.key == key)
        .map {
          case Nil => idx
          case _ => -1
        }
    }
  }

  def findIndex(hash: Int, key: A): Option[DictEntry[A, B]] = {
    val idx = hash & sizeMask
    val he = get(idx)

    if (he == null) None
    else {
      he.filter((e: DictEntry[A, B]) => e.key == key)
        .map {
          case entry :: Nil => entry
          case _ => null
        }
    }
  }

  def removeIndex(hash: Int, key: A): Option[DictEntry[A, B]] = {
    val idx = hash & sizeMask
    val he = get(idx)

    if (he == null) None
    else {
      val filtered = he.filter((e: DictEntry[A, B]) => e.key == key)

      filtered.map {
        case entry :: Nil => {
          table(idx) = he
            .filter((e: DictEntry[A, B]) => e.key != key)
            .map {
              case h :: t => h
              case Nil => null
            }
            .getOrElse(None.asInstanceOf[DictEntry[A, B]])

          used -= 1
          entry
        }
        case _ => null
      }
    }
  }
}
