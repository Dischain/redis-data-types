package ds

import scala.collection.mutable

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
    val hash = hashString(key)
    var idx: Int = -1

    for (table <- dict) {
      idx = hash & table.sizeMask
      var he = table.get(idx)

      while (he != null) {
        if (key == he.key) return None
        he = he.next
      }
    }

    Some(idx)
  }

  def dictAdd(key: A, value: B): Option[DictEntry[A, B]] = {
    var he: Option[DictEntry[A, B]] = null.asInstanceOf[Option[DictEntry[A, B]]]

    if (dictIsRehashing) dictRehashStep

    dictKeyIndex(key) match {
      case None => he = None
      case Some(index) if index != -1 => {
        val ht = if (dictIsRehashing) dict(1) else dict(0)
        val entry = new DictEntry[A, B](key, value)

        entry.next = ht.get(index)
        ht.set(index, entry)
        he = Some(entry)
      }
    }

    he
  }

  def dictResize: Option[Int] = {
    var minimal = dict(0).used

    if (minimal < initialSize) minimal = initialSize

    dictExpand(minimal)
  }

  def dictExpand(size: Int): Option[Int] = {
    var nht: DictHT[A, B] = null.asInstanceOf[DictHT[A, B]]

    val realSize: Int = _nextPower(size)

    if ((dictIsRehashing || dict(0).used > size) || (realSize == dict(0).size))
      return Some(0)

    nht = new DictHT[A, B](realSize)

    if (dict(0).used == 0) {
      dict(0) = nht
      return Some(1)
    }

    dict(1) = nht
    rehashIdx = 0
    return Some(1)
  }

  def dictRehash(n: Int): Option[Int] = {
    var emptyVisits = n * 10
    var nsteps = n
    var res: Option[Int] = null.asInstanceOf[Option[Int]]
    if (dictIsRehashing) res = Some(0)

    while (nsteps >= 0 && dict(0).used != 0) {
      var de: DictEntry[A, B] = null.asInstanceOf[DictEntry[A, B]]
      var nde: DictEntry[A, B] = null.asInstanceOf[DictEntry[A, B]]


      assert(dict(0).size > rehashIdx)
      while (dict(0).get(rehashIdx) == null) {
        rehashIdx += 1
        emptyVisits -= 1
        if (emptyVisits == 0) res = Some(1)
      }

      de = dict(0).get(rehashIdx)

      while (de != null) {
        nde = de.next

        var newIndex = hashString(de.key) & dict(1).sizeMask
        de.next = dict(1).get(newIndex)
        dict(1).set(newIndex, de)
        dict(0).used -= 1
        dict(1).used -= 1
        de = nde
      }

      dict(0).set(rehashIdx, null.asInstanceOf[DictEntry[A, B]])
      rehashIdx += 1
      nsteps -= 1
    }

    if (dict(0).used == 0) {
      dict(0) = dict(1)
      dict(1).clear
      res = Some(0)
    } else {
      res = Some(1)
    }

    res
  }

  def dictRehashStep: Int = dictRehash(1)
}

private object Dict {
  final val initialSize: Int = 4

  final val defaultLoadFactor: Int = 750

  final val loadFactorDenum: Int = 1000

  final def newThreshold(_loadFactor: Int, size: Int): Int =
    ((size.toLong * _loadFactor) / loadFactorDenum).toInt

  final def _nextPower(n: Int): Int = n << 1
}



