import scala.collection.immutable.Stream.cons
sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, b) => Some(a))

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _                   => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t)          => this
    case _                   => Empty
  }

//   def takeWhile(p: A => Boolean): Stream[A] = this match {
//     case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
//     case _                    => Empty
//   }
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      if (p(a)) Stream.cons(a, b)
      else Stream.empty
    })

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => {
      if (!p(a)) false
      else b
    })

  def map[B](p: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(p(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight((Stream.empty[A]))((a, b) => {
      if (p(a)) Stream.cons(a, b)
      else b
    })
  }

  def append[B >: A](x: => B): Stream[B] = {
    foldRight(Stream(x))((a, b) => Stream.cons(a, b))
  }

  def flatMap[B](p: A => Stream[B]): Stream[B] = {
    val x = foldRight(Stream.empty[Stream[B]])((a, b) => Stream.cons(p(a), b))
    Stream.flatten(x)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def flatten[A](as: Stream[Stream[A]]): Stream[A] = {
    def addStream(a: Stream[A], b: => Stream[A]) =
      a.foldRight(b)((aa, bb) => Stream.cons(aa, bb))
    as.foldRight(Stream.empty[A])(addStream)
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from[A](n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
}
