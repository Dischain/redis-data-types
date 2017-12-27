package ds

sealed trait List[+A] {
  def head: A

  def tail: List[A]

  def isEmpty: Boolean

  def length: Int

  def apply(i: Int): Option[A] = {
    if (isEmpty || i < 0 || i >= length - 1) None
    else if (i == 0) Some(head)
    else tail(i - 1)
  }

  def prepend[B >: A](item: B): List[B] =
    Cons(item, this)

  def append[B >: A](item: B): List[B] = {
    if (isEmpty) Cons(item, Nil)
    else Cons(head, tail.append(item))
  }

  def delete[B >: A](item: B): List[B] = {
    if (isEmpty)
      throw new UnsupportedOperationException("Can not delete on empty list")
    else if (item != head) Cons(head, tail.delete(item))
    else tail
  }

  def contains[B >: A](item: B): Boolean = {
    if (isEmpty) false
    else if (item == head) true
    else tail.contains(item)
  }

  def forEach(f: A => Unit): Unit = {
    if (!isEmpty) {
      f(head)
      tail.forEach(f)
    }
  }
}

final case class Cons[A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty: Boolean = false

  override def length: Int = 1 + tail.length
}

case object Nil extends List[Nothing] {
  override def head: Nothing =
    throw new NoSuchElementException("Empty list")

  override def tail: List[Nothing] =
    throw new NoSuchElementException("Empty list")

  override def isEmpty: Boolean = true

  override def length: Int = 0
}

object List {
  def apply[A](xs: A*): List[A] = {
    var acc: List[A] = Nil
    xs.foreach((item) => acc = acc.append(item))
    acc
  }
}