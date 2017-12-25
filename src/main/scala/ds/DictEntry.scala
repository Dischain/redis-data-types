package ds

import scala.annotation.tailrec

private[ds] final class DictEntry[A, B] (val key: A, var value: B) {
  var next: DictEntry[A, B] = _

  def filter(f: DictEntry[A, B] => Boolean): Option[List[DictEntry[A, B]]] = {
    val builder = new DictEntryBuilder[A, B]

    @tailrec
    def loop(
              acc: DictEntryBuilder[A, B],
              left: DictEntry[A, B],
              f: DictEntry[A, B] => Boolean): DictEntryBuilder[A, B] =
    {
      if (f(left)) acc += left
      else loop(acc, left, f)
    }

    loop(builder, this, f).result()
  }
}
