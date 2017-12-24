package ds

private[ds] final class DictEntry[A, B] (val key: A, var value: B) {
  var next: DictEntry[A, B] = _
}
