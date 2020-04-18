import scala.collection.immutable.Stream.cons
sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, b) => Some(a))

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = {
    Stream.unfold((n, this))(
      s =>
        s match {
          case (n, Cons(a, b)) if n > 0 => Some((a(), (n - 1, b())))
          case _                        => None
        }
    )
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t)          => this
    case _                   => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(
      s =>
        s match {
          case Cons(a, b) if (p(a())) => Some((a(), b()))
          case _                      => None
        }
    )

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
    Stream.unfold(this) {
      case Cons(a, b) => Some((p(a()), b()))
      case _          => None
    }
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

  def zip[B](s2: Stream[B]): Stream[(A, B)] = {
    Stream.unfold((this, s2)) {
      case (Cons(a, b), Cons(aa, bb)) => Some(((a(), aa()), (b(), bb())))
      case _ => None
    }
  }

  def find(f: A => Boolean): Option[A] = {
    this.filter(f).headOption
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, s2)) {
      case (Cons(a, b), Cons(aa, bb)) => Some((f(a(), aa()), (b(), bb())))
      case _                          => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case (Cons(a, b), Cons(aa, bb)) =>
        Some(((Some(a()), Some(aa())), (b(), bb())))
      case (_, Cons(aa, bb)) => Some(((None, Some(aa())), (Stream.empty, bb())))
      case (Cons(a, b), _)   => Some(((Some(a()), None), (b(), Stream.empty)))
      case _                 => None
    }
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

  def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def ones(): Stream[Int] = constant(1)

  def from[A](n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs(): Stream[Int] = {
    unfold((0, 1))(
      (z) =>
        z match {
          case (a, b) => Some((a, (b, a + b)))
        }
    )
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None         => Stream.empty
    }
  }
}
