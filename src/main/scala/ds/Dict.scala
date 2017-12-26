package ds

final class Dict[A, B] {
  import Dict._
  import util.MurmurHash2._

  private val dict = Array.fill[DictHT[A, B]](2)(new DictHT[A, B](initialSize))

  private var threshold: Int = newThreshold(defaultLoadFactor, dict(0).size)

  private var rehashIdx: Int = -1

  private def dictSlots: Int = dict(0).size + dict(1).size
  private def dictUsed: Int = dict(0).used + dict(1).used
  private def dictIsRehashing: Boolean = rehashIdx != -1

  private def dictKeyIndex(key: A): Option[Int] = {
    val hash: Int = hashString(key.toString)

    if (dictIsRehashing) dict(1).filterIndex(hash, key)
    else dict(0).filterIndex(hash, key)
  }

  def dictAdd(key: A, value: B): Option[DictEntry[A, B]] = {
    if (dictIsRehashing) dictRehash()

    dictKeyIndex(key) match {
      case Some(-1) => None
      case Some(index) => {
        val ht = if (dictIsRehashing) dict(1) else dict(0)
        val entry = new DictEntry[A, B](key, value)

        entry.next = ht.get(index)
        ht.put(index, entry)
        Some(entry)
      }
      case _ => None
    }
  }

  def dictDelete(key: A): Option[DictEntry[A, B]] = {
    if (dict(0).used != 0 && dict(0).used != 0) {
      if (dictIsRehashing) dictRehash()

      val h: Int = hashString(key.toString)

      if (dictIsRehashing) dict(1).removeIndex(h, key)
      else dict(0).removeIndex(h, key)
    } else None
  }

  def dictFind(key: A): Option[DictEntry[A, B]] = {
    if (dictUsed == 0) None
    else {
      if (dictIsRehashing) dictRehash()

      val h: Int = hashString(key.toString)

      val res = for {
        table <- dict
      } yield table.findIndex(h, key)

      res.head
    }
  }

  private[ds] def dictResize = {
    var minimal = dict(0).used

    if (minimal < initialSize) minimal = initialSize

    dictExpand(minimal)
  }

  private def dictExpand(size: Int) = {
    val realSize: Int = _nextPower(size)

    if (!(dictIsRehashing || dict(0).used > size) && !(realSize == dict(0).size)) {
      val nht: DictHT[A, B] = new DictHT[A, B](realSize)

      if (dict(0).used == 0) {
        dict(0) = nht
      } else {
        dict(1) = nht
        rehashIdx = 0
      }
    }
  }

  private def dictRehash(n: Int = 1): Unit = {
    var emptyVisits = n * 10
    var nsteps = n

    if (dictIsRehashing) {
      while (nsteps > 0 && dict(0).used != 0) {
        while (dict(0).get(rehashIdx) == null || emptyVisits != 0) {
          rehashIdx += 1
          emptyVisits -= 1
        }

        dict(0).get(rehashIdx).forEach((de: DictEntry[A, B]) => {
          val newIndex: Int = hashString(de.key.toString) & dict(1).sizeMask
          dict(1).put(newIndex, de)
          dict(0).used -= 1
          dict(1).used += 1
        })

        dict(0).put(rehashIdx, null)
        rehashIdx += 1
        nsteps -= 1
      }

      if (dict(0).used == 0) {
        dict(0) = dict(1)
        dict(1).clear
      }
    }
  }
}

private object Dict {
  final val initialSize: Int = 4

  final val defaultLoadFactor: Int = 750

  final val loadFactorDenum: Int = 1000

  final def newThreshold(_loadFactor: Int, size: Int): Int =
    ((size.toLong * _loadFactor) / loadFactorDenum).toInt

  final def _nextPower(n: Int): Int = n << 1
}



