package ds

//sealed abstract class RBTree[+A: Ordered, B] {
sealed abstract class RBTree[+A <% Ordered[A], B] {
  def color: Color

  def key: A

  def value: B

  def left: RBTree[A, B]

  def right: RBTree[A, B]

  def isEmpty: Boolean

  def add[C >: A <% Ordered[C]](k: C, v: B): RBTree[C, B] = {
    blacken(selectBranch(k, v))
  }

  def get[C >: A <% Ordered[C]](k: C): Option[B] = {
    if (k < key) left.get(k)
    else if (k > key) right.get(k)
    else Option(value)
  }

  private def selectBranch[C >: A <% Ordered[C]](k: C, v: B): RBTree[C, B] = {
    if (k < key) balance(Tree(color, left.selectBranch(k, v), key, value, right))
    else if (k == key) Tree(color, left, k, v, right)
    else balance(Tree(color, left, key, value, right.selectBranch(k, v)))
  }

  private def blacken[C >: A <% Ordered[C]](node: RBTree[C, B]) = node match {
    case Tree(_, l, k, v, r) => Tree(Black, l, k, v, r)
    case _ => node
  }

  private def balance[C >: A <% Ordered[C]](node: RBTree[C, B]) = node match {
    // black parent with left red child, which has left red child
    case Tree(Black,Tree(Red,Tree(Red,l1,k1,v1,r1),k2,v2,r2),k3,v3,r3) =>
      Tree(Red,Tree(Black,l1,k1,v1,r1),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with left red child, which has right red child
    case Tree(Black,Tree(Red,l1,k1,v1,Tree(Red,l2,k2,v2,r2)),k3,v3,r3) =>
      Tree(Red,Tree(Black,l1,k1,v1,l2),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with right red child, which has left red child
    case Tree(Black,l1,k1,v1,Tree(Red,Tree(Red,l2,k2,v2,r2),k3,v3,r3)) =>
      Tree(Red,Tree(Black,l1,k1,v1,l2),k2,v2,Tree(Black,r2,k3, v3,r3))
    // black parent with right red child, which has right red child
    case Tree(Black,l1,k1,v1,Tree(Red,l2,k2,v2,Tree(Red,l3,k3,v3,r3))) =>
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

case object Leaf extends RBTree[Nothing, Nothing] {
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

object RBTree {
  def apply[A <% Ordered[A], B](item: (A, B)): RBTree[A, B] = {
    val t: RBTree[A, B] = Leaf[Nothing, Nothing]
    t.add(item._1, item._2)
    t
  }
}