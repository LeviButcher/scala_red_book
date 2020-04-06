package fpinscala.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil         => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](ds: List[A], x: A): List[A] = ds match {
    case Nil         => Cons(x, Nil)
    case Cons(_, xs) => Cons(x, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _                   => as
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((curr, acc) => acc + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((acc, curr) => Cons(curr, acc))
  }

  def append[A](as: List[A], a: A) = {
    foldRight(as, Cons(a, Nil))(Cons(_, _))
  }

  def flatten[A](as: List[List[A]]): List[A] = {
    var addList = (a: List[A], b: List[A]) => foldRight(a, b)(Cons(_, _))
    foldLeft(as, Nil: List[A])(addList)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((curr, acc) => Cons(f(curr), acc))
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    var x = foldRight(as, Nil: List[List[B]])((curr, acc) => Cons(f(curr), acc));
    flatten(x)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {

    def returnIfTrue(x: A): List[A] = {
      if (f(x)) {
        return List(x)
      }
      List()
    }

    flatMap(as)(returnIfTrue)
  }
}
