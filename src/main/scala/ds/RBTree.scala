package ds

sealed abstract class RBTree[+A: Ordered, B] {
  def color: Color

  def key: A

  def value: B

  def left: RBTree[A, B]

  def right: RBTree[A, B]

  def add[C >: A <% Ordered[A]](k: C, v: B): RBTree[C, B] = this match {
    case n @ Tree(c, l, k, v, r) => {
      if (k < key) balance(n) // ? ...
    }
    case _ => Tree(Red, this, k, v, this)
  }

  private[this] def balance(node: RBTree[A, B]) = node match {
    // black parent with left red child, which has left red child
    case (Black,Tree(Red,Tree(Red,l1,k1,v1,r1),k2,v2,r2),k3,v3,r3) =>
      Tree(Red,Tree(Black,l1,k1,v1,r1),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with left red child, which has right red child
    case (Black,Tree(Red,l1,k1,v1,Tree(Red,l2,k2,v2,r2)),k3,v3,r3) =>
      Tree(Red,Tree(Black,l1,k1,v1,l2),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with right red child, which has left red child
    case (Black,l1,k1,v1,Tree(Red,Tree(Red,l2,k2,v2,r2),k3,v3,r3)) =>
      Tree(Red,Tree(Black,l1,k1,v1,l2),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with right red child, which has right red child
    case (Black,l1,k1,v1,Tree(Red,l2,k2,v2,Tree(Red,l3,k3,v3,r3))) =>
      Tree(Red,Tree(Black,l1,k1,v1,l2),k2,v2,Tree(Black,l3,k3, v3,r3))
    case _ => node
  }
}

final case class Tree[+A: Ordered, B](
  color: Color,
  left: RBTree[A, B],
  key: A,
  value: B,
  right: RBTree[A, B]) extends RBTree[A, B]
{
  def isEmpty = false
}

final case object Leaf extends RBTree[Nothing, Nothing] {
  def color: Color = Black

  def key = throw new NoSuchElementException("Empty leaf")

  def value = throw new NoSuchElementException("Empty leaf")

  def left = throw new NoSuchElementException("Empty leaf")

  def right = throw new NoSuchElementException("Empty leaf")

  def isEmpty = true
}

sealed trait Color
case object Red extends Color
case object Black extends Color