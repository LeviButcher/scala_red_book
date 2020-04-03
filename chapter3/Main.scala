import fpinscala.datastructure._

object Main {
  def main(args: Array[String]): Unit = {
    val og: List[Int] = List(1, 2, 4)
    println(og)
    var temp = List.tail(og)
    println(temp)
    var empty = List.tail(List())
    println(empty)
    println(List.setHead(temp, 5))
    println(List.drop(og, 2))
    println(List.dropWhile(og)(x => x < 4))

    var add = (a: Int, b: Int) => a + b

    println("Test Fold Right")
    println(List.foldRight(List(1, 2, 4), 0)(add))

    println("Test Length")
    println(List.length(og))

    println("Test Fold Left")
    println(List.foldLeft(List(1, 2, 4), 0)(add))

    println("Test Reverse")
    println(List.reverse(List(1, 2, 4)))

    println("Test Append")
    println(List.append(List(1, 2, 4), 5))

    println("Test Flatten")
    var listyList = List(List(1), List(2), List(3))
    println(List.flatten(listyList))

    println("Test Map")
    println(List.map(List(1, 2, 3))(x => x + 1))

    println("Test FlatMap")
    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

    println("Test Filter")
    println(List.filter(List(1, 2, 3, 1))(i => i % 2 != 0))
  }
}
