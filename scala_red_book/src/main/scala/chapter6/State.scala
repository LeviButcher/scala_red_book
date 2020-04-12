object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => unit(f(a)))
  // s1 => {
  //   val (a, s2) = s(s1)
  //   (f(a), s2)
  // }

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(
      f: (A, B) => C
  ): State[S, C] = { s1 =>
    {
      val (a, s2) = ra(s1)
      val (b, s3) = rb(s2)
      (f(a, b), s3)
    }
  }

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] =
    s1 => {
      val (a, s2) = f(s1)
      g(a)(s2)
    }

  // def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {}
}
