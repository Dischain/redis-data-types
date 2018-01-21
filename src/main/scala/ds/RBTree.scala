package ds

import scala.runtime.Nothing$

sealed trait RBTree[+A, B] {

  implicit def orderingByC[C >: A]: Ordering[C] = new Ordering[C] {
    override def compare(x: C, y: C): Int = x.## - y.##

    override def lt(x: C, y: C): Boolean = super.lt(x, y)

    override def gt(x: C, y: C): Boolean = super.gt(x, y)

    override def equiv(x: C, y: C): Boolean = super.equiv(x, y)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }

  def add[C >: A](k: C, v: B): RBTree[C, B] = {
    blacken(genericAdd(k, v))
  }

  def delete[C >: A](k: C): RBTree[C, B] = {
    val nodeParent = getNodeParent(k, initial = this)
    nodeParent match {
      case Tree(_, l: Tree[C, A], key, value, r: Tree[C, A]) => bstDeletion(k)
      case Leaf() => this
      case _ => rbDeletion(k, this)
    }
  }

  def get[C >: A](k: C): Option[B]

  protected[ds] def getNodeParent[C >: A](k: C, initial: RBTree[C, B]): RBTree[C, B]

  protected[ds] def genericAdd[C >: A](k: C, v: B): RBTree[C, B]

  protected[ds] def bstDeletion[C >: A](k: C): RBTree[C, B]

  protected[ds] def rbDeletion[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B]

  protected def getMinChildOf[C >: A](node: RBTree[C, B], prev: RBTree[C, B]): (C, B) = node match {
    case Tree(_, Leaf(), k: C, v: B, Leaf()) => (k, v)
    case Tree(_, l: Tree[C, B], _, _, _) => getMinChildOf(l, this)
    case _ => prev match { case Tree(_, _, k: C, v: B, _) => (k, v) }
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
    case Tree(_, l: Tree[C, B], _, _, _) => getMin(l)
    case _ => node
  }

  private def blacken[C >: A](node: RBTree[C, B]): RBTree[C, B] = node match {
    case Leaf() => node
    case Tree(_, l, k, v, r) => Tree(Black, l, k, v, r)
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

  def genericAdd[C >: A](k: C, v: B): RBTree[C, B] = {
    if (implicitly[Ordering[C]].lt(k, key))
      balance(Tree(color, left.genericAdd(k, v), key, value, right))
    else if (implicitly[Ordering[C]].gt(k, key))
      balance(Tree(color, left, key, value, right.genericAdd(k, v)))
    else Tree(color, left, k, v, right)
  }

  // return node to be deleted with parent node
  def getNodeParent[C >: A](k: C, initial: RBTree[C, B]): RBTree[C, B] = {
    if (implicitly[Ordering[C]].lt(k, key))
      Tree(color, left.getNodeParent(k, this), key, value, right)
    else if (implicitly[Ordering[C]].gt(k, key))
      Tree(color, left, key, value, right.getNodeParent(k, this))
    else
      initial
  }

  def bstDeletion[C >: A](k: C): RBTree[C, B] = {
    if (implicitly[Ordering[C]].lt(k, key)) Tree(color, left.bstDeletion(k), key, value, right)
    else if (implicitly[Ordering[C]].gt(k, key)) Tree(color, left, key, value, right.bstDeletion(k))
    else {
      val (minK, minV) = getMinChildOf(left, this)
      if (!implicitly[Ordering[C]].lt(minK, key) && !implicitly[Ordering[C]].gt(minK, key))
        Leaf[C, B]()
      else Tree(color, left.bstDeletion(minK), minK, minV, right)
    }
  }

  def rbDeletion[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B] = {
    println("rbDeletion on key: " + k)
    if (implicitly[Ordering[C]].lt(k, key)) Tree(color, left.rbDeletion(k, this), key, value, right)
    else if (implicitly[Ordering[C]].gt(k, key)) Tree(color, left, key, value, right.rbDeletion(k, this))
    else handleRbCases(parent)
  }

  def handleRbCases[C >: A](parent: RBTree[C, B]): RBTree[C, B] = parent match {
    // case 1.1
    case Tree(c, Tree(Black, ll, lk, lv, Leaf()), k, v, r)
      if implicitly[Ordering[B]].eq(lk, key) => ll match {
      case Tree(Red, Leaf(), ck, cv, Leaf()) =>
        Tree(c, Tree(Black, Leaf(), ck, cv, Leaf()), k, v, r)
    }
    // case 1.2
    case Tree(c, Tree(Black, Leaf(), lk, lv, lr), k, v, r)
      if implicitly[Ordering[B]].eq(lk, key) => lr match {
      case Tree(Red, Leaf(), ck, cv, Leaf()) =>
        Tree(c, Tree(Black, Leaf(), ck, cv, Leaf()), k, v, r)
    }
    // case 1.3
    case Tree(c, l, k, v, Tree(Black, rl, rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) => rl match {
      case Tree(Red, Leaf(), ck, cv, Leaf()) =>
        Tree(c, l, k, v, Tree(Black, Leaf(), ck, cv, Leaf()))
    }
    // case 1.4
    case Tree(c, l, k, v, Tree(Black, Leaf(), rk, rv, rr))
      if implicitly[Ordering[B]].eq(rk, key) => rr match {
      case Tree(Red, Leaf(), ck, cv, Leaf()) =>
        Tree(c, l, k, v, Tree(Black, Leaf(), ck, cv, Leaf()))
    }
    // case 1.5
    case Tree(c, Tree(Red, ll, lk, lv, Leaf()), k, v, r)
      if implicitly[Ordering[B]].eq(lk, key) => ll match {
      case Tree(Black, Leaf(), ck, cv, Leaf()) =>
        Tree(c, Tree(Black, Leaf(), ck, cv, Leaf()), k, v, r)
    }
    // case 1.6
    case Tree(c, Tree(Red, Leaf(), lk, lv, lr), k, v, r)
      if implicitly[Ordering[B]].eq(lk, key) => lr match {
      case Tree(Black, Leaf(), ck, cv, Leaf()) =>
        Tree(c, Tree(Black, Leaf(), ck, cv, Leaf()), k, v, r)
    }
    // case 1.7
    case Tree(c, l, k, v, Tree(Red, Leaf(), rk, rv, rr))
      if implicitly[Ordering[B]].eq(rk, key) => rr match {
      case Tree(Black, Leaf(), ck, cv, Leaf()) =>
        Tree(c, l, k, v, Tree(Black, Leaf(), ck, cv, Leaf()))
    }
    // case 1.8
    case Tree(c, l, k, v, Tree(Red, rl, rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) => rl match {
      case Tree(Black, Leaf(), ck, cv, Leaf()) =>
        Tree(c, l, k, v, Tree(Black, Leaf(), ck, cv, Leaf()))
    }

    // case 2.a.1.1
    case Tree(_, Tree(Black, Leaf(), lk, lv, Leaf()), k, v,
      Tree(Black, slc @ Tree(Red, Leaf(), slck, slcv, Leaf()), rk, rv,
      src @ Tree(Red, Leaf(), srck, srcv, Leaf()))
    )
      if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black,
        Tree(Black,
          Leaf(),
          k, v,
          Tree(Red, Leaf(), slck, slcv, Leaf())
        ),
        rk, rv,
        Tree(Black, Leaf(), srck, srcv, Leaf())
      )
    // case 2.a.1.2
    case Tree(_, Tree(Black, Leaf(), lk, lv, Leaf()), k, v,
    Tree(Black, slc @ Leaf(), rk, rv,
    src @ Tree(Red, Leaf(), srck, srcv, Leaf())))
    if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Tree(Black, Leaf(), k, v, Leaf()),
        rk, rv,
        Tree(Black, Leaf(), srck, srcv, Leaf())
      )
    // case 2.a.2
    case Tree(_, Tree(Black, Leaf(), lk, lv, Leaf()), k, v,
    Tree(Black, slc @ Tree(Red, Leaf(), slck, slcv, Leaf()), rk, rv,
    src @ Leaf()))
      if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Tree(Black, Leaf(), k, v, Leaf()), slck, slcv, Tree(Black, Leaf(), rk, rv, Leaf()))
    // case 2.a.3.1
    case Tree(Black,
      Tree(Black,
        slc @ Tree(Red, Leaf(), slck, slcv, Leaf()),
        lk, lv,
        src @ Tree(Red, Leaf(), srck, srcv, Leaf())
      ),
      k, v,
      Tree(Black, Leaf(), rk, rv, Leaf())
    )
    if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black,
        Tree(Black, Leaf(), slck, slcv, Leaf()),
        lk, lv,
        Tree(Black,
          Tree(Red, Leaf(), srck, srcv, Leaf()),
            k, v,
          Leaf()
        )
      )
    // case 2.a.3.2
    case Tree(Black,
    Tree(Black,
    slc @ Tree(Red, Leaf(), slck, slcv, Leaf()),
      lk, lv,
    src @ Leaf()),
    k, v,
    Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black,
        Tree(Black, Leaf(), slck, slcv, Leaf()),
        lk, lv,
        Tree(Black,
          Leaf(),
            k, v,
          Leaf()
        )
      )
    // case 2.a.4
    case Tree(Black,
    Tree(Black, slc @ Leaf(), lk, lv, src @ Tree(Red, Leaf(), srck, srcv, Leaf())),
      k, v,
    Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black,
        Tree(Black, Leaf(), lk, lv, Leaf()),
          srck, srcv,
        Tree(Black, Leaf(), k, v, Leaf())
      )
    // case 2.b.1
    case Tree(Black, Tree(Black, Leaf(), lk, lv, Leaf()), k, v, Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Leaf(), k, v, Tree(Red, Leaf(), rk, rv, Leaf()))
    // case 2.b.2
    case Tree(Black, Tree(Black, Leaf(), lk, lv, Leaf()), k, v, Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black, Tree(Red, Leaf(), lk, lv, Leaf()), k, v, Leaf())
    // case 2.b.3
    case Tree(Red, Tree(Black, Leaf(), lk, lv, Leaf()), k, v, Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Leaf(), k, v, Tree(Red, Leaf(), rk, rv, Leaf()))
    // case 2.b.4
    case Tree(Red, Tree(Black, Leaf(), lk, lv, Leaf()), k, v, Tree(Black, Leaf(), rk, rv, Leaf()))
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black, Tree(Red, Leaf(), lk, lv, Leaf()), k, v, Leaf())
    // case 2.c.1
    case Tree(Black,
    Tree(Black, Leaf(), lk, lv, Leaf()),
    k, v,
    Tree(Red,
    slc @ Tree(Black, Leaf(), slck, slcv, Leaf()),
      rk, rv,
    src @ Tree(Black, Leaf(), srck, srcv, Leaf())
    )
    )
      if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black,
        Tree(Black, Leaf(), k, v, Tree(Red, Leaf(), slck, slcv, Leaf())),
        rk, rv,
        Tree(Black, Leaf(), srck, srcv, Leaf())
      )
    // case 2.c.2
    case Tree(Black,
    Tree(Black, Leaf(), lk, lv, Leaf()),
      k, v,
    Tree(Red,
    Tree(Black, Leaf(), slck, slcv, Leaf()),
    rk, rv,
    Leaf()
    )
    )
    if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Tree(Black, Leaf(), k, v, Leaf()), slck, slcv, Tree(Black, Leaf(), rk, rv, Leaf()))
    // case 2.c.3 <-- check
    case Tree(Black,
      Tree(Black, Leaf(), lk, lv, Leaf()),
      k, v,
      Tree(Red,
        Tree(Black, Leaf(), srck, srcv, Leaf()),
        rk, rv,
        Leaf())
    )
    if implicitly[Ordering[B]].eq(lk, key) =>
      Tree(Black, Tree(Black, Leaf(), k, v, Leaf()), rk, rv, Tree(Black, Leaf(), srck, srcv, Leaf()))
    // case 2.c.4
    case Tree(Black,
    Tree(Red,
    Tree(Black, Leaf(), slck, slcv, Leaf()),
    lk, lv,
    Tree(Black, Leaf(), srck, srcv, Leaf())
    ),
    k, v,
    Tree(Black, Leaf(), rk, rv, Leaf())
    )
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black,
        Tree(Black, Leaf(), slck, slcv, Leaf()),
        lk, lv,
        Tree(Black,
          Tree(Red, Leaf(), srck, srcv, Leaf()),
          k, v,
          Leaf()
        )
      )
    // case 2.c.5
    case Tree(Black,
    Tree(Red,
    Tree(Black, Leaf(), slck, slcv, Leaf()),
    lk, lv,
    Leaf()
    ),
    k, v,
    Tree(Black, Leaf(), rk, rv, Leaf())
    )
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black, Tree(Black, Leaf(), slck, slcv, Leaf()), lk, lv, Tree(Black, Leaf(), k, v, Leaf()))
    // case 2.c.6
    case Tree(Black,
    Tree(Red,
    Leaf(),
    lk, lv,
    Tree(Black, Leaf(), srck, srcv, Leaf())
    ),
    k, v,
    Tree(Black, Leaf(), rk, rv, Leaf())
    )
      if implicitly[Ordering[B]].eq(rk, key) =>
      Tree(Black, Tree(Black, Leaf(), srck, srcv, Leaf()), lk, lv, Tree(Black, Leaf(), k, v, Leaf()))
  }
}



final case class Leaf[A, B]() extends RBTree[A, B] {
  def get[C >: Nothing](k: C): Option[Nothing] = None

  def genericAdd[C >: A](k: C, v: B): RBTree[C, B] = {
    Tree(Red, this, k, v, this)
  }

  def getNodeParent[C >: A](k: C, initial: RBTree[C, B]): RBTree[C, B] = initial

  def bstDeletion[C >: A](k: C): RBTree[C, B] = this

  def rbDeletion[C >: A](k: C, parent: RBTree[C, B]): RBTree[C, B] = this
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