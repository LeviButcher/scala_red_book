import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => {
      lazyUnit(f(a))
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es => {
    unit(ps.map(a => a(es).get()))(es)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val s = as.map(asyncF(x => if (f(x)) List(x) else List()))
    map(sequence(s))(_.flatten)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(es => if (run(es)(cond).get()) unit(0)(es) else unit(1)(es))(
      List(t, f)
    )

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
    {
      val i = run(es)(n).get()
      choices(i)(es)
    }
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val k = run(es)(key).get()
    choices.get(k).get(es)
  }

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    run(es)(a).get()(es)
  }
}
