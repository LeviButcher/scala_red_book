trait Parsers[ParseError, Parer[+_]] {
  self =>
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOPs[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOPs(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def map[A, B](p: Parser[A], f: A => B)
  def combine[A, B](p: Parser[A], p: Parser[B]): Parser[(A, B)]
  def many[A](p: Parser[A]): Parser[List[A]]
  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](p: Parser[A], p: Parser[B]): Parser[(A, B)]
  def map2[A, B, C](p: Parser[A], p: Parser[B])(f: (A, B) => C): Parser[C]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    val numA: Parser[Int] = char('a').many.map(_.size)
  }
}
