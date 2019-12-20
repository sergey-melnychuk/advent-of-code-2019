import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val line = Source.fromFile("input.txt").getLines.toList.head
val p = line.split(",").toList.map(_.toInt)

val in = ArrayBuffer[Long]()
val out = ArrayBuffer[Long]()
val x = new Intcode(p, in, out, 1024)

def comb[T](choices: List[T]): List[List[T]] = {
  def sub(current: List[T], remaining: List[T], f: List[T] => Unit): Unit = {
    if (remaining.isEmpty) f(current)
    else {
      remaining.foreach(r => {
        val rest = remaining.filterNot(_ == r)
        sub(r :: current, rest, f)
      })
    }
  }

  var acc: List[List[T]] = Nil
  val f: List[T] => Unit = (current) => acc = current :: acc

  sub(Nil, choices, f)
  acc
}

def run(program: List[Int], code: Long, input: Long = 0): Long = {
  val x = new Intcode(program, in = ArrayBuffer(code, input), out = ArrayBuffer(), 1024)
  x.run
  println(s"halt: ${x.halt} blocked: ${x.blocked}")
  x.out.head
}

def chain(init: List[Long], program: List[Int]): List[Long] = {
  val (_, acc) = init.foldLeft((0L, List.empty[Long]))((tuple, item) => tuple match {
    case (in, acc) => 
      val out = run(program, item, in)
      (out, out :: acc)
  })
  acc
}

comb(List(0,1,2,3,4).map(_.toLong))
  .map(x => chain(x, p))
  .map(_.head)
  .max

// 101490

// Part 2

def loop(init: List[Long], program: List[Int]): Long = {
  val ps = init.map(code => new Intcode(program, in = ArrayBuffer(code), out = ArrayBuffer[Long](), 1024))
  
  def iter(signal: Long): Long = {
    ps.foldLeft(signal)((s, p) => {
      p.in += s
      p.run
      p.out.last
    })
  }

  var in: Long = 0
  while (ps.exists(p => !p.halt)) {
    val out = iter(in)
    in = out
  }

  in
}

comb(List(5,6,7,8,9).map(_.toLong))
  .map(x => loop(x, p))
  .max

// 61019896

