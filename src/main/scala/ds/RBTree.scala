package ds

sealed trait RBTree[+A, B] {

  implicit def orderingByC[C >: A]: Ordering[C] = new Ordering[C] {
    override def compare(x: C, y: C): Int = x.## - y.##

    override def lt(x: C, y: C): Boolean = super.lt(x, y)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }

  def add[C >: A](k: C, v: B): RBTree[C, B] = {
    blacken(selectBranch(k, v))
  }

  def get[C >: A](k: C): Option[B]

  def delete[C >: A](k: C): Option[B]

  private[ds] def selectBranch[C >: A](k: C, v: B): RBTree[C, B]

  private[ds] def blacken[C >: A](node: RBTree[C, B]): RBTree[C, B] = node match {
    case Leaf() => node
    case Tree(_, l, k, v, r) => Tree(Black, l, k, v, r)
  }

  private[ds] def balance[C >: A](node: RBTree[C, B]): RBTree[C, B] = node match {
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

final case class Tree[+A : Ordering, B](
  color: Color,
  left: RBTree[A, B],
  key: A,
  value: B,
  right: RBTree[A, B]) extends RBTree[A, B]
{
  val isEmpty = false

  def get[C >: A](k: C): Option[B] = {
    if (implicitly[Ordering[C]].lt(k, key)) left.get(k)
    else if (implicitly[Ordering[C]].gt(k, key)) right.get(k)
    else Option(value)
  }

  def delete[C >: A](k: C): Option[B] = ???

  private[ds] def selectBranch[C >: A](k: C, v: B): RBTree[C, B] = {
    if (implicitly[Ordering[C]].lt(k, key))
      balance(Tree(color, left.selectBranch(k, v), key, value, right))
    else if (implicitly[Ordering[C]].equals(k, key))
      Tree(color, left, k, v, right)
    else balance(Tree(color, left, key, value, right.selectBranch(k, v)))
  }
}

final case class Leaf[A, B]() extends RBTree[A, B] {
  def get[C >: Nothing](k: C): Option[Nothing] = None

  def delete[C >: Nothing](k: C): Option[Nothing] = None

  private[ds] def selectBranch[C >: A](k: C, v: B): RBTree[C, B] = {
    Tree(Red, this, k, v, this)
  }
}

sealed trait Color
case object Red extends Color
case object Black extends Color

object RBTree {
  def apply[A : Ordering, B](item: (A, B)): RBTree[A, B] = {
    val t: RBTree[A, B] = Leaf()
    t.add(item._1, item._2)
  }
}