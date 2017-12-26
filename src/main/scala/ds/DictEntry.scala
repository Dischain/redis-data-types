package ds

import scala.annotation.tailrec

/*private[ds]*/ final class DictEntry[A, B] (val key: A, var value: B) {
  var next: DictEntry[A, B] = _

  def filter(f: DictEntry[A, B] => Boolean): Option[List[DictEntry[A, B]]] = {
    val builder = new DictEntryBuilder[A, B]

    @tailrec
    def loop(
              acc: DictEntryBuilder[A, B],
              left: DictEntry[A, B]): DictEntryBuilder[A, B] =
    {
        if (left != null && f(left)) acc += left
        else if (left == null) acc
        else loop(acc, left.next)
    }

    loop(builder, this).result()
  }

  def forEach(f: DictEntry[A, B] => Unit) = {
    @tailrec
    def loop(e: DictEntry[A, B]): Unit = {
      f(e)
      if (e.next != null) loop(e.next)
    }

    loop(this)
  }
}
