sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
object Machine {

  def simulateMachine(inputs: List[Input]): State.State[Machine, (Int, Int)] = {
    (x) =>
      inputs.foldRight(((x.coins, x.candies), x))(
        (i, acc) => machineProcess(i)(acc._2)
      )
  }

  def machineProcess(i: Input): State.State[Machine, (Int, Int)] =
    State.map2(unlockIfCoin(i), dispenseIfTurn(i))(
      (a, b) => (b)
    )

  def unlockIfCoin(i: Input): State.State[Machine, (Int, Int)] = { x =>
    i match {
      case Coin =>
        ((x.coins + 1, x.candies), Machine(false, x.candies, x.coins + 1))
      case _ => State.unit((x.coins, x.candies))(x)
    }
  }

  def dispenseIfTurn(i: Input): State.State[Machine, (Int, Int)] = { x =>
    i match {
      case Turn if !x.locked && x.candies > 0 =>
        ((x.coins, x.candies - 1), Machine(true, x.candies - 1, x.coins))
      case _ => State.unit((x.coins, x.candies))(x)
    }
  }
}
