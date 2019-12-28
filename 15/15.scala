import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val line = Source.fromFile("input.txt").getLines.toList.head
val p = line.split(",").toList.map(_.toLong)

val x = new Intcode(p, ArrayBuffer[Long](), ArrayBuffer[Long](), p.size * 3)

sealed trait Dir { val code: Long }
object N extends Dir { val code = 1L }
object S extends Dir { val code = 2L }
object W extends Dir { val code = 3L }
object E extends Dir { val code = 4L }

def readDir(code: Long): Dir = {
  val array = Array(N, S, W, E)
  val index = code.toInt - 1
  array(index)
}

sealed trait Cell { val code: Long }
object Wall  extends Cell { val code = 0L }
object Move  extends Cell { val code = 1L }
object Found extends Cell { val code = 2L }
object Empty extends Cell { val code = -1L }

def readCell(code: Long): Cell = {
  val array = Array(Wall, Move, Found)
  array(code.toInt)
}

case class Pos(x: Int, y: Int) {
  def move(dir: Dir): Pos = {
    dir match {
      case N => Pos(x, y - 1)
      case S => Pos(x, y + 1)
      case W => Pos(x - 1, y)
      case E => Pos(x + 1, y)
    }
  }

  def dir(to: Pos): Dir = {
    (to.x - x, to.y - y) match {
      case (-1,  0) => W
      case ( 1,  0) => E
      case ( 0, -1) => N
      case ( 0,  1) => S
      case _ => throw new RuntimeException(s"No direction from $this to $to")
    }
  }

  def dist(to: Pos): Double = {
    math.sqrt((to.x-x) * (to.x-x) + (to.y-y)*(to.y-y))
  }
}

type World = Map[Pos, Cell]

def moves(pos: Pos, last: Pos, world: World): List[Dir] = {
  def weight(cell: Cell): Int = {
    cell match {
      case Found => 0
      case Empty => 1
      case Move => 2
      case Wall => 3
    }
  }

  List(N, S, W, E)
    .filter(d => pos.move(d) != last)
    .map(d => {
      val next = pos.move(d)
      val cell = world.getOrElse(next, Empty)
      //println(s"pos=$pos dir=$d next=$next cell=$cell")
      (d, cell)
    })
    .filter(_._2 != Wall)
    .sortBy(x => weight(x._2))
    .map(_._1)
}

@annotation.tailrec
def restore(pos: Pos, path: Map[Pos, Pos], acc: List[Pos] = Nil): List[Pos] = {
  if (!path.contains(pos)) pos :: acc
  else {
    restore(path(pos), path, pos :: acc)
  }
}

def remote(x: Intcode, dir: Dir): Cell = {
  x.in += dir.code
  x.run
  val code = x.out.head
  x.out.clear()
  readCell(code)
}

class Robot(x: Intcode) {
  var world = Map.empty[Pos, Cell]
  var path = Map.empty[Pos, Pos]
  var cost = Map.empty[Pos, Int]
  var hits = Map.empty[Pos, Int]

  var pos = Pos(0,0)
  var last = pos
  world = world + (pos -> Move)
  cost = cost + (pos -> 0)
  hits = hits + (pos -> 1)
  
  def hit(p: Pos): Unit = {
    hits = hits + (p -> (hits.getOrElse(p, 0) + 1))
  }

  def step(): Cell = {
    val dirs = moves(pos, last, world)
    val dir = if (dirs.isEmpty) pos.dir(last) else dirs.head
    val cell = remote(x, dir)
    val next = pos.move(dir)

    println(s"robot: pos=$pos dir=${dir.code} next=$next cell=${cell.code}")
     
    if (cell == Move || cell == Found) {
      val costPos = cost(pos)
      val costNext = cost.getOrElse(next, Int.MinValue)
      
      val betterCost = costNext > (costPos + 1)
      val noPath = !cost.contains(next)
      if (betterCost || noPath) {
        path = path + (next -> pos)
        cost = cost + (next -> (costPos + 1))
      }

      hit(pos)
      last = pos
      pos = next
    }

    world = world + (next -> cell)
    cell
  }

  def find(): Int = {
    while (step() != Found) {}

    cost(pos)
  }

  def empty(): List[Pos] = {
    world.keySet
      .toList
      .filter(p => world.getOrElse(p, Empty) == Move)
      .flatMap(p => moves(p, p, world).map(d => p.move(d)))
      .toSet
      .filterNot(p => world contains p)
      .toList
  }

  def source(): Pos = {
    world.toList.find(_._2 == Found).map(_._1).get
  }

  def goal(ps: List[Pos]): Pos = {
    if (ps.isEmpty) pos
    else ps.sortBy(p => p.dist(pos)).head
  }

  def explore(): Unit = {
    //val target = goal(empty())
    val dirs = moves(pos, last, world)
    val dir = if (dirs.nonEmpty) dirs.sortBy(d => hits.getOrElse(pos.move(d), 0)).head
              else pos.dir(last)
    val cell = remote(x, dir)
    val next = pos.move(dir)
    world = world + (next -> cell)

    //println(s"robot.expore: pos=$pos dir=${dir.code} next=$next cell=${cell.code}")
    if (cell == Move || cell == Found) {
      hit(pos)
      last = pos
      pos = next
    }
  }

  def show(seen: Set[Pos] = Set()): String = {
    val ps = world.keySet.toList
    val xs = ps.map(_.x)
    val ys = ps.map(_.y)

    def chr(x: Int, y: Int): Char = {
      val p = Pos(x, y)
      if (seen(p)) 'O' else
      if (p == pos) '@' else
      world.getOrElse(p, Empty) match {
        case Empty => ' '
        case Wall => '#'
        case Move => '.'
        case Found => 'X'
      }
    }

    def line(y: Int, lo: Int, hi: Int): String = {
      (lo to hi).toList.map(x => chr(x, y)).mkString
    }

    def grid(xs: List[Int], ys: List[Int]): String = {
      val (lo, hi) = (xs.min, xs.max)
      (ys.min to ys.max).toList.map(y => line(y, lo, hi)).mkString("\n")
    }

    grid(xs, ys)
  }

  def adj(p: Pos): List[Pos] = {
    List(N, S, W, E).map(d => p.move(d))
  }

  def fill(source: Pos): Int = {
    @annotation.tailrec
    def iter(n: Int, queue: List[Pos], seen: Set[Pos]): Int = {
      //println
      //println(s"n=$n\n" + show(seen))

      if (queue.isEmpty) n - 1
      else {
        val next = queue
          .flatMap(p => {
            adj(p).filterNot(seen)
              .filter(p => world(p) == Move)
          })

        iter(n + 1, next, seen ++ queue)
      }
    }

    iter(0, List(source), Set(source))
  }
}

val robot = new Robot(x)
val steps = robot.find()
// 234

println(robot.show())
println

while(!robot.empty.isEmpty) { 
  robot.explore
} 

println(robot.show())

val source = robot.source()
robot.fill(source)
// 292

