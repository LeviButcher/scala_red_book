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
}
