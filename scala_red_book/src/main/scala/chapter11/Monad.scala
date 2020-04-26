trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  // def map[A, B](ma: F[A])(f: A => B): F[B] =
  //   flatMap(ma)(a => unit(f(a)))
  // def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
  //   flatMap(ma)(a => map(mb)(b => f(a, b)))

  // def sequence[A](lma: List[F[A]]): F[List[A]] = {
  //   lma.foldLeft(unit(List.empty[A]))(
  //     (acc, curr) => map2(acc, curr)((a, b) => b :: a)
  //   )
  // }
  // def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
  //   sequence(la.map(f))
  // }
  // def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
  //   sequence(List.fill(n)(ma))
  // }
  // def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))(
      (x, y) =>
        compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x)
    )

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = { a =>
    flatMap(f(a))(g)
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(x => x)
  }
}

object Monad {
  val optionMonad = new Monad[Option] {
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma match {
        case Some(a) => f(a)
        case _       => None
      }
    def unit[A](a: => A) = None
  }
}
