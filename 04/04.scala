// input: 158126-624574

val lo = 158126
val hi = 624574

def fits(digits: List[Int]): Boolean = {
  val diffs = digits.zip(digits.tail).map({ case (a, b) => b - a })

  diffs.forall(_ >= 0) && diffs.contains(0)
}

def digits(x: Int): List[Int] = {
  @annotation.tailrec
  def sub(x: Int, acc: List[Int] = Nil): List[Int] = {
    if (x == 0) acc
    else {
      val d = x % 10
      val r = x / 10
      sub(r, d :: acc)
    }
  }

  sub(x)
}

def solve(lo: Int, hi: Int, f: Int => Boolean): Int = {
  (lo to hi).filter(f).size
}

solve(lo, hi, x => fits(digits(x)))
// 1665

// Part 2

def part2(digits: List[Int]): Boolean = digits.groupBy(x => x).values.map(_.size).toSet.contains(2)

solve(lo, hi, x => fits(digits(x)) && part2(digits(x)))
// 1131

