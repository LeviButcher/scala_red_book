class CandyDispenserTest extends org.scalatest.FunSuite {
  test("Not Locked, Out of Candy, do nothing") {
    var x = Machine(false, 0, 5);
    var (_, res) = Machine.simulateMachine(List(Coin, Turn))(x);
    assert(res == Machine(false, 0, 6));
  }

  test("Unlock locked machine, when there is candy") {
    var x = Machine(true, 5, 5)
    var (_, res) = Machine.simulateMachine(List(Coin))(x);
    assert(res == Machine(false, 5, 6))
  }

  test("Dispense Candy, when unlocked and have candy, then become locked") {
    var x = Machine(false, 5, 5)
    var (_, res) = Machine.simulateMachine(List(Turn))(x)
    assert(res == Machine(true, 4, 5))
  }

  test("Complex scenario, should return expected") {
    var x = Machine(true, 5, 10);
    var inputs = List(Turn, Coin, Turn, Turn, Coin, Turn)
    var (left, machine) = Machine.simulateMachine(inputs)(x)
    assert(left == (12, 3))
    assert(machine == Machine(true, 3, 12))
  }
}
