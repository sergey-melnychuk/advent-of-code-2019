
Part 1:
import scala.io.Source
val lines = Source.fromFile("01-input.txt").getLines().toList

lines.filterNot(_.isEmpty).map(_.toLong).map(_ / 3).map(_ - 2).sum

Answer: 3393938

Part 2:
def fuel(mass: Long): Long = mass / 3 - 2

@annotation.tailrec
def unfold(mass: Long, acc: List[Long] = Nil): List[Long] = {
  val f = fuel(mass)
  if (f <= 0) acc.reverse
  else unfold(f, f :: acc)
}

val ms = lines.filterNot(_.isEmpty).map(_.toLong)

ms.flatMap(unfold).sum

Answer: 5088037

