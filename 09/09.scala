import scala.io.Source

val line = Source.fromFile("input.txt").getLines.toList.head
val p = line.split(",").toList.map(_.toInt)

val x = new Intcode(p, 1, 1024 * 1024)
x.run()
x.msg

// 2399197539

// Part 2

val x = new Intcode(p, 2, 1024 * 1024)
x.run()
x.msg

// 35106

