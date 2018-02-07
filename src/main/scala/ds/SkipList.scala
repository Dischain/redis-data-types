package ds

class SkipList[A: Ordering, B](val maxLevel: Int = 10) {
  private var numLevels: Int = 0

  def insert(k: A, v: B): Option[B] = {
    def insertR(node: SLNode[A, B], newNode: SLNode[A, B], level: Int): Option[B] = {
      val key = newNode.key
      val next = node.forward(level)

      if (next.isEmpty || implicitly[Ordering[A]].lt(key, next.key)) {
        if (level < newNode.level) {
          newNode.forward (level) = next
          node.forward (level) = newNode
        }
        if (level != 0) insertR(node, newNode, level - 1)
        else None
      }
      else insertR(next, newNode, level)
    }

    val nn: SLNode[A, B] = new SLNode[A, B](k, v, randomLevel())

    if (head.isEmpty) {
      head = new SLNode[A, B](k, v, maxLevel)
      numLevels += 1
      Some(v)
    }
    else insertR(head, nn, numLevels)
  }

  private var head: SLNode[A, B] = new SLNode[A, B]()

  def remove(k: A): Option[B] = {
    println(s"current numLevels: $numLevels")
    def removeR(node: SLNode[A, B], k: A, level: Int): Option[B] = {
      println("node size: " + node.forward.length)
      println("current level: " + level)
      if (node.forward.length < level) return removeR(node, k, level - 1)
      val next = node.forward(level)

      println(s"next.key: ${next.key}, k: $k")
      if (implicitly[Ordering[A]].gteq(next.key, k)) {
        if (!implicitly[Ordering[A]].lt(next.key, k) && !implicitly[Ordering[A]].gt(next.key, k))
          node.forward(level) = next.forward(level)
        if (level == 0) {
          node.forward(level) = new SLNode[A, B]()
          Some(next.value)
        }
        else removeR(node, k, level - 1)
      }
      else removeR(node.forward(level), k, level)
    }

    if (head.isEmpty) None
    else removeR(head, k, numLevels)
  }

  def find(k: A): Option[B] = {
    def findR(node: SLNode[A, B], k: A, level: Int): Option[B] = {
      if (k == node.key) Some(node.value)
      else {
        val next: SLNode[A, B] = node.forward(level)

        if (next.isEmpty || implicitly[Ordering[A]].lt(k, next.key)) {
          if (level != 0) findR(node, k, level - 1)
          else None
        }
        else findR(next, k, level)
      }
    }

    if (head.isEmpty) throw new NoSuchElementException("Empty node")
    else findR(head, k, numLevels)
  }

  private def randomLevel(): Int = {
    var i = 0
    var j = 2
    val randFactor = Math.pow(2, maxLevel - 1).toInt
    val randVal = new scala.util.Random().nextInt(randFactor) + 1

    while (i < maxLevel - 1 && randVal.toInt <= randFactor / j) {
      i += 1; j += j
    }
    if (i > numLevels) numLevels = i
    i
  }
}

class SLNode[A: Ordering, B] (val key: A, val value: B, val level: Int, val isEmpty: Boolean = false) {
  def this() {
    this(null.asInstanceOf[A], null.asInstanceOf[B], -1, true)
  }

  val forward: Array[SLNode[A, B]] =
    Array.fill[SLNode[A, B]](level)(new SLNode[A, B]())

  def apply(key: A, value: B, level: Int): SLNode[A, B] =
    new SLNode[A, B](key, value, level)
}