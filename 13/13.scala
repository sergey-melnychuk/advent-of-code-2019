import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val line = Source.fromFile("input.txt").getLines.toList.head
val program = line.split(",").toList.map(_.toLong)

val x = new Intcode(program, ArrayBuffer[Long](), ArrayBuffer[Long](), program.size * 10)

x.run

x.out.toList.grouped(3).filter(_(2) == 2).size
// 230

// Part 2

val p = {
  val a = program.toArray
  a(0) = 2L
  a.toList
}

case class Pos(x: Int, y: Int)
case class Tile(pos: Pos, id: Int)

val x = new Intcode(p, ArrayBuffer[Long](), ArrayBuffer[Long](), p.size * 10)

def print(input: List[Tile]): String = {
  val tiles = input.filter(_.pos.x >= 0)

  val xs = tiles.map(_.pos.x)
  val ys = tiles.map(_.pos.y)

  val map = tiles.filter(_.pos.x >= 0).map(t => (t.pos, t.id)).toMap

  def chr(id: Int): Char = {
    id match {
      case 0 => ' '
      case 1 => '#'
      case 2 => '*'
      case 3 => '='
      case 4 => '@' // ball
      case _ => '?'
    }
  }

  def line(y: Int): String = {
     (xs.min to xs.max).map(x => {
        val p = Pos(x, y)
        chr(map(p))
     }).mkString
  }

  (ys.min to ys.max).map(line).mkString("\n")
}

def tiles(x: Intcode): List[Tile] = {
  val in = x.out.map(_.toInt)
  x.out.clear()
  in.grouped(3).toList.map(three => {
    val ArrayBuffer(x, y, id) = three
    Tile(Pos(x, y), id)
  })
}

def blocks(map: Map[Pos, Int]): Int = map.values.count(_ == 2)

def locate(map: Map[Pos, Int], id: Int): List[Pos] = map.toList.filter(_._2 == id).map(_._1)

def input(map: Map[Pos, Int]): Int = {
  val List(ball) = locate(map, 4)
  val List(pad) = locate(map, 3)
  if (pad.x == ball.x) 0 else if (pad.x > ball.x) -1 else 1
}

class Game(x: Intcode) {
  x.run
  var map: Map[Pos, Int] = tiles(x).map(t => (t.pos, t.id)).toMap
  var score: Int = 0

  def move(): Unit = {
     val in = input(map)
     x.in += in
     x.run
     val ts = tiles(x)
     val inc = ts.filter(_.pos.x >= 0).map(t => (t.pos, t.id)).toMap
     map = map ++ inc
     score = ts.find(_.pos.x < 0).map(_.id).getOrElse(score)
  }

  def debug(): Unit = {
    val ts = map.toList.map(kv => Tile(kv._1, kv._2))
    println(print(ts) + s"\nscore: $score")
  }

  def run(): Int = {
    while(!x.halt && blocks(map) > 0) {
      move()
      //debug()
    }
    score
  }
}

val game = new Game(x)
game.run()
// 11140

