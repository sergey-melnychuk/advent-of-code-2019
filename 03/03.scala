import scala.io.Source
val lines = Source.fromFile("input.txt").getLines.toList.filter(_.nonEmpty)

case class Point(row: Int, col: Int) {
  def move(dir: Char): Point = dir match {
    case 'R' => Point(row, col + 1)
    case 'L' => Point(row, col - 1)
    case 'U' => Point(row - 1, col)
    case 'D' => Point(row + 1, col)
    case _ => this
  }

  def dist(to: Point): Int = math.abs(row - to.row) + math.abs(col - to.col)
}

case class Line(dir: Char, len: Int)

def parseLine(op: String): Line = {
  Line(op.charAt(0), op.drop(1).toInt)
}

def parseWire(wire: String): List[Line] = {
  wire.split(",").toList.map(parseLine)
}

def chars(ls: List[Line]): List[Char] = {
  ls.flatMap(line => (1 to line.len).map(_ => line.dir).toList)
}

def points(from: Point, cs: List[Char]): List[Point] = {
  @annotation.tailrec
  def collect(cs: List[Char], p: Point, acc: List[Point]): List[Point] = {
    cs match {
      case Nil => acc.reverse
      case c :: rest => 
        val next = p.move(c)
        collect(rest, next, next :: acc)
    }
  }
  collect(cs, from, Nil)
}


val List(a, b) = lines

val zero = Point(0, 0)
val pa = points(zero, chars(parseWire(a)))
val pb = points(zero, chars(parseWire(b)))

val sa = pa.toSet
val sb = pb.toSet

val ab = sa.intersect(sb).toList
ab.map(p => p.dist(zero)).min
// 225

// Part 2:

def index(ps: List[Point]): Map[Point, Int] = {
  var map = scala.collection.mutable.Map[Point, Int]()
  ps.zipWithIndex.foreach({
    case (p, i) if (!map.contains(p)) => map.put(p, i + 1)
    case _ =>
  })
  map.toList.toMap
}

val ia = index(pa)
val ib = index(pb)

ab.map(p => ia(p) + ib(p)).min
// 35194

