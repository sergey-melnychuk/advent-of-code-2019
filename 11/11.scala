import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val line = Source.fromFile("input.txt").getLines.toList.head
val program = line.split(",").toList.map(_.toLong)

val x = new Intcode(program, in = ArrayBuffer[Long](), out = ArrayBuffer[Long](), 1024 * 10)

sealed trait Dir
object N extends Dir
object S extends Dir
object W extends Dir
object E extends Dir

sealed trait Turn { val code: Long }
object L extends Turn { val code = 0L }
object R extends Turn { val code = 1L }

sealed trait Color { val code: Long }
object Black extends Color { val code = 0L }
object White extends Color { val code = 1L }

def parseColor(x: Long): Color = {
  if (x == Black.code) Black else White
}

def parseTurn(x: Long): Turn = {
  if (x == L.code) L else R
}

def makeTurn(d: Dir, t: Turn): Dir = {
  (d, t) match {
    case (N, L) => W
    case (S, L) => E
    case (W, L) => S
    case (E, L) => N
    case (N, R) => E
    case (S, R) => W
    case (W, R) => N
    case (E, R) => S
  }
}

case class Pos(x: Int, y: Int) {
  def step(dir: Dir): Pos = {
    dir match {
      case N => Pos(x, y - 1)
      case S => Pos(x, y + 1)
      case W => Pos(x - 1, y)
      case E => Pos(x + 1, y)
    }
  }
}

class Robot(var pos: Pos, var dir: Dir, var painted: Map[Pos, Color], x: Intcode) {
   
  def step(): Unit = {
    val currentColor = painted.getOrElse(pos, Black)
    x.in += currentColor.code
    x.run
    val ArrayBuffer(colorCode, turnCode) = x.out
    x.out.clear()
    val (color, turn) = (parseColor(colorCode), parseTurn(turnCode))
    // println(s"$pos $currentColor -> $color $turn ${x.halt} ${x.err} ${x.msg}")
    painted = painted + (pos -> color)
    dir = makeTurn(dir, turn)
    pos = pos.step(dir)
  }

  def run(): Map[Pos, Color] = {
    while (!x.halt) {
      step()
    }
    painted
  } 
}

val robot = new Robot(Pos(0, 0), N, Map.empty[Pos, Color], x)
val cells = robot.run

cells.keySet.size
// 1885

// Part 2

val x2 = new Intcode(program, ArrayBuffer[Long](), ArrayBuffer[Long](), 1024 * 10)

val zero = Pos(0, 0)
val r2 = new Robot(zero, N, Map.empty[Pos, Color] + (zero -> White), x2)

val nr = r2.run.toList.filter(_._2 == White).map(_._1)

val xs = nr.map(_.x)
val ys = nr.map(_.y)

val cs = for { y <- ys.min to ys.max; x <- xs.min to xs.max; val c: Char = if (nr.contains(Pos(x,y))) 'X' else ' ' } yield c

cs.toList.grouped((xs.max - xs.min + 1)).toList.map(_.mkString).foreach(println)

/*
XXX  XXXX XXXX  XX   XX  X  X  XX  XXXX
X  X X    X    X  X X  X X  X X  X X
XXX  XXX  XXX  X  X X    XXXX X  X XXX
X  X X    X    XXXX X XX X  X XXXX X
X  X X    X    X  X X  X X  X X  X X
XXX  X    XXXX X  X  XXX X  X X  X X
*/

// BFEAGHAF

