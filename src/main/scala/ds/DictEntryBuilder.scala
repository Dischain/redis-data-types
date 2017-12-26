package ds

private[ds] final class DictEntryBuilder[A, B] {
  private var storage: List[DictEntry[A, B]] = Nil

  def +=(e: DictEntry[A, B]): DictEntryBuilder[A, B] = {
    storage match {
      case head :: tail => e.next = head
      case _ =>
    }
    storage = e :: storage
    this
  }

  def reset(): DictEntryBuilder[A, B] = {
    storage = null
    this
  }

  def result() = Option(storage)
}