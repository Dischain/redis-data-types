package ds

sealed trait RBTree[+A, B] {

  implicit def orderingByC[C >: A]: Ordering[C] = new Ordering[C] {
    override def compare(x: C, y: C): Int = x.## - y.##

    override def lt(x: C, y: C): Boolean = super.lt(x, y)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }

  def add[C >: A](k: C, v: B): RBTree[C, B] = {
    blacken(genericAdd(k, v))
  }

  def get[C >: A](k: C): Option[B]

  def delete[C >: A](k: C): RBTree[C, B] = {
    val temp = genericDelete(k, this)
    temp
  }

  protected[ds] def genericDelete[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B]

  protected[ds] def genericAdd[C >: A](k: C, v: B): RBTree[C, B]

  private[this] def blacken[C >: A](node: RBTree[C, B]): RBTree[C, B] = node match {
    case Leaf() => node
    case Tree(_, l, k, v, r) => Tree(Black, l, k, v, r)
  }

  protected[ds] def handleDelition[C >: A](node: RBTree[C, B]): RBTree[C, B] = {
    println("handle delition: " + node)
    node
  }

  protected def balance[C >: A](node: RBTree[C, B]): RBTree[C, B] = {
    node match {
      // black parent with left red child, which has left red child
      case Tree(Black, Tree(Red, Tree(Red, l1, k1, v1, r1), k2, v2, r2), k3, v3, r3) =>
        Tree(Red, Tree(Black, l1, k1, v1, r1), k2, v2, Tree(Black, r2, k3, v3, r3))
      // black parent with left red child, which has right red child
      case Tree(Black, Tree(Red, l1, k1, v1, Tree(Red, l2, k2, v2, r2)), k3, v3, r3) =>
        Tree(Red, Tree(Black, l1, k1, v1, l2), k2, v2, Tree(Black, r2, k3, v3, r3))
      // black parent with right red child, which has left red child
      case Tree(Black, l1, k1, v1, Tree(Red, Tree(Red, l2, k2, v2, r2), k3, v3, r3)) =>
        Tree(Red, Tree(Black, l1, k1, v1, l2), k2, v2, Tree(Black, r2, k3, v3, r3))
      // black parent with right red child, which has right red child
      case Tree(Black, l1, k1, v1, Tree(Red, l2, k2, v2, Tree(Red, l3, k3, v3, r3))) =>
        Tree(Red, Tree(Black, l1, k1, v1, l2), k2, v2, Tree(Black, l3, k3, v3, r3))
      case _ => node
    }
  }
  protected def getMin[C >: A](node: RBTree[C, B]): RBTree[C, B] = node match {
    case Tree(_, Leaf(), _, _, _) => node
    case Tree(_, l, _, _, _) => getMin(l)
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

  def get[C >: A](k: C): Option[B] = {
    if (implicitly[Ordering[C]].lt(k, key)) left.get(k)
    else if (implicitly[Ordering[C]].gt(k, key)) right.get(k)
    else Option(value)
  }

  protected[ds] def genericAdd[C >: A](k: C, v: B): RBTree[C, B] = {
    if (implicitly[Ordering[C]].lt(k, key))
      balance(Tree(color, left.genericAdd(k, v), key, value, right))
    else if (implicitly[Ordering[C]].gt(k, key))
      balance(Tree(color, left, key, value, right.genericAdd(k, v)))
    else Tree(color, left, k, v, right)
  }

  // return node to be deleted with parent node
  protected[ds] def genericDelete[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B] = {
    println("generic delete for " + k + " (" + k.## + ") when key is " + key + " (" + key.## + ")")

    if (implicitly[Ordering[C]].lt(k, key)) {
      println("lt, k: " + k + " key: " + key)
      Tree(color, left.genericDelete(k, this), key, value, right)
    }
    else if (implicitly[Ordering[C]].gt(k, key)) {
      println("gt, k: " + k + " key: " + key)
      Tree(color, left, key, value, right.genericDelete(k, this))
    }
    else {
      println("eq, k: " + k + " key: " + key)
      handleDelition(parent)
    }
  }
}

final case class Leaf[A, B]() extends RBTree[A, B] {
  def get[C >: Nothing](k: C): Option[Nothing] = None

  protected[ds] def genericAdd[C >: A](k: C, v: B): RBTree[C, B] = {
    Tree(Red, this, k, v, this)
  }

  protected[ds] def genericDelete[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B] = {
    this
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