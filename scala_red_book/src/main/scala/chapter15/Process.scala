sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) =>
      s match {
        case Cons(h, t) => recv(Some(h()))(t())
        case xs         => recv(None)(xs)
      }
    case Emit(h, t) => Stream.cons(h, (t(s)))
  }
  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def filter[I](p: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat
  }

}
case class Emit[I, O](
    head: O,
    tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Await[I, O](
    recv: Option[I] => Process[I, O]
) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }
  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => Halt()
      }
    go(0.0)
  }
  def take[I](n: Int): Process[I, I] = {
    def go(a: Int): Process[I, I] = {
      Await {
        case Some(d) if a > 0 => Emit(d, go(a - 1))
        case _                => Halt()
      }
    }
    go(n)
  }
  def drop[I](n: Int): Process[I, I] = {
    def go(a: Int): Process[I, I] = {
      Await {
        case Some(_) if a > 0 => go(a - 1)
        case Some(d)          => Emit(d, go(a - 1))
        case _                => Halt()
      }
    }
    go(n)
  }
}
