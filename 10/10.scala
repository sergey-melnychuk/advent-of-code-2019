import scala.io.Source
val lines = Source.fromFile("input.txt").getLines.toList

@annotation.tailrec
def gcd(a: Int, b: Int): Int = { 
  if (b == 0) a
  else gcd(b, a % b)
}

case class Vec(x: Int, y: Int) {
  def norm(): Vec = {
    val ax = math.abs(x)
    val ay = math.abs(y)
    val d = gcd(ax max ay, ax min ay)
    Vec(x / d, y / d)
  }
}

case class Asteroid(x: Int, y: Int) {
  def vec(that: Asteroid): Vec = Vec(that.x - x, that.y - y).norm()
  def dist(that: Asteroid): Double = math.sqrt((that.x - x) * (that.x - x) + (that.y - y) * (that.y - y))
}

val asteroids = lines.zipWithIndex.flatMap({
  case (line, y) => 
    line.zipWithIndex
      .filter({
        case (c, x) => c == '#'
      })
      .map({
        case (_, x) => Asteroid(x, y)
      })
})

def trace(center: Asteroid): List[(Asteroid, Vec)] = {
  asteroids.filter(_ != center)
    .map(a => (a, center.vec(a)))
}

asteroids.map(trace).map(_.map(_._2).toSet.size).max
// 253

val (center, part1) = asteroids.map(a => (a, trace(a).map(_._2).toSet.size)).maxBy(_._2)

// Part 2

case class Polar(vec: Vec, distance: Double)

// angle in radians from UP direction vector
def azimuth(vec: Vec): Double = {
  val d = math.sqrt(vec.x * vec.x + vec.y * vec.y)
  val (x, y) = (vec.x / d, vec.y / d)
  // sin(A) = x, then A = asin(x)
  val alpha = math.asin(x)
  if (y < 0) {
    if (alpha >= 0.0) alpha
    else 2 * math.Pi + alpha
  } else {
    math.Pi - alpha
  }
}

def vaporize(center: Asteroid, remaining: Set[Asteroid]): List[Asteroid] = {
  val targets: Map[Vec, Asteroid] = remaining.toList.groupBy(a => center.vec(a)).mapValues(as => as.minBy(x => center.dist(x)))
  targets.toList.sortBy(x => azimuth(x._1)).map(_._2)
}

@annotation.tailrec
def rotate(center: Asteroid, remaining: Set[Asteroid], acc: List[Asteroid]): List[Asteroid] = {
  if (remaining.isEmpty) acc
  else {
    val vaporized = vaporize(center, remaining)
    rotate(center, remaining -- vaporized.toSet, acc ++ vaporized)
  }
}

val order = rotate(center, asteroids.filterNot(_ == center).toSet, Nil)

val that = order.drop(199).head

that.x * 100 + that.y
// 815

