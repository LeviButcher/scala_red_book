object IsSorted{
  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(prev: Int, curr: Int) : Boolean =
      if(curr == as.length) true
      else if(ordered(as(prev), as(curr))) loop(prev + 1, curr + 1)
      else false

    loop(0, 1)
  }
}
