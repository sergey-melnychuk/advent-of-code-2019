import scala.io.Source
val line = Source.fromFile("input.txt").getLines.toList.head
val as = line.split(",").map(_.toInt)

def operation(op: Int, a: Int, b: Int): Int = {
  op match {
    case 1 => a + b
    case 2 => a * b
    case _ => 0
  }
}

def command(as: Array[Int], op: Int, a: Int, b: Int, c: Int): Unit = {
  as(c) = operation(op, as(a), as(b))
}

def process(as: Array[Int], at: Int): Option[Int] = {
  as(at) match {
    case 99 => None
    case op => {
      command(as, op, as(at+1), as(at+2), as(at+3))
      Some(at + 4)
    }
  }
}

def solve(a: Int, b: Int, as: Array[Int]): Int = {
  as(1) = a
  as(2) = b

  @annotation.tailrec
  def sub(at: Option[Int]): Int = {
    at match {
      case None => as(0)
      case Some(idx) => sub(process(as, idx))
    }
  }

  sub(Some(0))
}

solve(12, 2, as.clone()) 
// 3716293

// part 2

val answers = for {
  a <- 0 to 99
  b <- 0 to 99
} yield (a, b, solve(a, b, as.clone()))


answers.find(_._3 == 19690720).map(x => 100 * x._1 + x._2)
// Some(6429)
