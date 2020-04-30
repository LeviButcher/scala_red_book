class Chapter15 extends org.scalatest.FunSuite {
  test("Take") {
    val x = Process.take(2)(Stream(1.0, 2.0, 3.0, 4.0)).toList
    val y = Process.take(3).apply(Stream(1.0, 2.0)).toList
    assert(x == y)
  }
  test("drop") {
    val x = Process.drop(2)(Stream(1, 2, 3)).toList
    val y = List(3);
    assert(x == y)
  }

  test("take while") {
    val x = Process.takeWhile[Int](i => i % 2 == 0)(Stream(2, 4, 6, 5)).toList
    val y = List(2, 4, 6)
    assert(x == y)
  }

  test("Drop While") {
    val x = Process.dropWhile[Int](i => i % 2 == 0)(Stream(2, 4, 6, 5)).toList
    val y = List(5)
    assert(x == y)
  }
}
