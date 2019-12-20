
import scala.io.Source

val lines = Source.fromFile("input.txt").getLines.toList

case class V3(x: Int, y: Int, z: Int) {
  def +(that: V3): V3 = V3(this.x + that.x, this.y + that.y, this.z + that.z)

  private def cmp(a: Int, b: Int): Int = if (a == b) 0 else if (a < b) 1 else -1
  def cmp(that: V3): V3 = V3(cmp(x, that.x), cmp(y, that.y), cmp(z, that.z))

  def energy(): Int = math.abs(x) + math.abs(y) + math.abs(z)

  override def toString(): String = f"{$x%4d,$y%4d,$z%4d}"
}

case class Moon(p: V3, v: V3) {
  // apply velocity
  def move(): Moon = Moon(p + v, v)

  // apply gravity
  def pull(that: Moon): Moon = Moon(p, v + p.cmp(that.p))

  def energy(): Long = p.energy().toLong * v.energy().toLong

  override def toString(): String = s"$p;$v"
}

val moons = lines.map(line => {
    val chunks = line.substring(1, line.length - 1).split(",")
    val Array(x, y, z) = chunks.map(_.split("=")).map(_(1)).map(_.toInt)
    Moon(V3(x, y, z), V3(0, 0, 0))
  }).toArray

def pairs(n: Int): List[(Int, Int)] = {
  (for {
    i <- 0 until n
    j <- 0 until n
    if i != j
  } yield (i, j)).toList
}

def round(moons: Array[Moon]): Array[Moon] = {
  val ms = moons.clone()
  pairs(moons.size).foreach({
    case (i, j) => 
      val a = ms(i)
      val b = ms(j)
      ms(i) = a.pull(b)
  })

  ms.map(m => m.move())
}

val result = (1 to 1000).toList.foldLeft(moons)((ms, _) => round(ms))

result.map(_.energy()).sum
// 8538

// Part 2

def hash(moons: Array[Moon], get: V3 => Long): String = moons.map(m => get(m.p).toString + "," + get(m.v).toString).mkString(";")

// find number of steps for a new state to match any previous state
def repeats(moons: Array[Moon], max: Long, get: V3 => Long): Long = {
  val seen = new java.util.HashSet[String]()
  var next = moons
  var n: Long = 0
  while (n < max && !seen.contains(hash(next, get))) {
    seen.add(hash(next, get))
    next = round(next)
    n += 1
  }
  n
}

val test1 = Array(Moon(V3(-1, 0, 2), V3(0, 0, 0)), Moon(V3(2,-10,-7), V3(0,0,0)), Moon(V3(4,-8,8), V3(0,0,0)), Moon(V3(3,5,-1), V3(0,0,0)))
val test2 = Array(Moon(V3(-8,-10,0), V3(0,0,0)),Moon(V3(5,5,10), V3(0,0,0)),Moon(V3(2,-7,3), V3(0,0,0)),Moon(V3(9,-8,-3), V3(0,0,0)))

def three(moons: Array[Moon]): (Long, Long, Long) = {
  val max: Long = 1000000
  (repeats(moons, max, _.x), repeats(moons, max, _.y), repeats(moons, max, _.z))
}

def isPrime(x: Long): Boolean = {
  val max = math.sqrt(x).toInt
  (2 to max).forall(d => x % d > 0)
}

def primes(x: Long): Map[Long, Int] = {
  @annotation.tailrec
  def sub(rem: Long, acc: List[Long] = Nil): List[Long] = {
    val max = math.sqrt(rem).toInt + 1
    (2 to max).map(_.toLong).filter(isPrime).find(d => rem % d == 0) match {
       case None => rem :: acc
       case Some(d) => sub(rem / d, d :: acc)
    }
  }

  sub(x).groupBy(x => x).mapValues(_.size)
}

def merge(a: Map[Long, Int], b: Map[Long, Int]): Map[Long, Int] = {
  (a.keySet ++ b.keySet).toList.map(k => (k, a.getOrElse(k, 0) max b.getOrElse(k, 0))).toMap
}

def asMap(x: Long, y: Long, z: Long): Map[Long, Int] = {
   List(x, y, z).foldLeft(Map.empty[Long, Int])((map, p) => merge(map, primes(p)))
}

def exp(v: Long, n: Int): Long = (1 to n).foldLeft(1L)((acc, _) => acc * v)

def mults(m: Map[Long, Int]) = m.toList.map({ case (v, n) => exp(v, n) })

def part2(moons: Array[Moon]): Long = {
  val (x, y, z) = three(moons)
  mults(asMap(x, y, z)).foldLeft(1L)((acc, x) => acc * x)
}

part2(moons)
// 506359021038056

