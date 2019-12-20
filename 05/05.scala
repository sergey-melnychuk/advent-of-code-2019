import scala.io.Source

val line = Source.fromFile("input.txt").getLines.toList.head
val program = line.split(",").toList.map(_.toInt)

val intcode = new Intcode(program, io = 1)
intcode.run()
intcode.io
// 13087969

// Part 2
val p2 = new Intcode(program, io = 5)
p2.run()
p2.io
// 14110739

