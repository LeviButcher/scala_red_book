sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
object Machine {

  def simulateMachine(inputs: List[Input]): State.State[Machine, (Int, Int)] = {
    (x) =>
      val i = inputs.head
      unlockIfCoin(i)(x)
  }

  def unlockIfCoin(i: Input): State.State[Machine, (Int, Int)] = { x =>
    i match {
      case Coin =>
        ((x.coins, x.candies), Machine(false, x.candies, x.coins + 1))
      case _ => State.unit((x.coins, x.candies))(x)
    }
  }
}
