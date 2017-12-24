package util

object MurmurHash2 {
  def hashString[A <: String](key: A): Int = {
    val m = 0x5bd1e995
    val seed = 0
    val r = 24

    var len = key.length
    var h = seed ^ len

    var data = key
    var k = 0

    while (len >= 4) {
      k  = data(0).toInt
      k |= data(1).toInt << 8
      k |= data(2).toInt << 16
      k |= data(3).toInt << 24

      k *= m
      k ^= k >> r
      k *= m

      h *= m
      h ^= k

      data += 4
      len -= 4
    }

    len match {
      case 3 => h ^= data(2).toInt << 16
      case 2 => h ^= data(1).toInt << 8
      case 1 => h ^= data(0).toInt; h *= m
    }

    h ^= h >> 13
    h *= m
    h ^= h >> 15

    h
  }
}
