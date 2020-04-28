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
}
