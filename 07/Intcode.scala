// Advent of Code 2019

import scala.collection.mutable.ArrayBuffer

sealed trait Arg
case class Val(value: Long) extends Arg
case class Mem(addr: Int) extends Arg
case class Rel(rel: Int) extends Arg

sealed trait Op
case class Add(one: Arg, two: Arg, res: Arg) extends Op
case class Mul(one: Arg, two: Arg, res: Arg) extends Op

case class In(arg: Arg) extends Op
case class Out(arg: Arg) extends Op

case class JumpTrue(one: Arg, two: Arg) extends Op
case class JumpFalse(one: Arg, two: Arg) extends Op
case class Less(one: Arg, two: Arg, res: Arg) extends Op
case class Equal(one: Arg, two: Arg, res: Arg) extends Op

case class RelBase(arg: Arg) extends Op

case object Halt extends Op

// Based on 'Day 15' version
class Intcode(program: List[Int], var in: ArrayBuffer[Long], var out: ArrayBuffer[Long], memory: Int = 1024) {
  val mem: Array[Long] = program.map(_.toLong).toArray ++ Array.fill(memory - program.size)(0.toLong)
  var at: Int = 0
  var rel: Int = 0
  var err: Boolean = false
  var msg: String = ""
  var halt: Boolean = false
  var blocked: Boolean = false

  def fetch(): Option[Op] = {
    val code = mem(at).toInt % 100
    val opts = mem(at).toInt / 100
    val (a, b, c) = ((opts / 100) % 10, (opts / 10) % 10, opts % 10)
    val argOne: Int => Arg = (cell: Int) => if (c == 2) Rel(mem(cell).toInt) else if (c == 1) Val(mem(cell)) else Mem(mem(cell).toInt)
    val argTwo: Int => Arg = (cell: Int) => if (b == 2) Rel(mem(cell).toInt) else if (b == 1) Val(mem(cell)) else Mem(mem(cell).toInt)
    val argRes: Int => Arg = (cell: Int) => if (a == 2) Rel(mem(cell).toInt) else Mem(mem(cell).toInt)
    code match {
       case  1 =>
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         val res = argRes(at + 3)
         at += 4
         Some(Add(one, two, res))
       
       case  2 => 
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         val res = argRes(at + 3)
         at += 4
         Some(Mul(one, two, res))

       case  3 =>
         val one = argOne(at + 1)
         at += 2
         Some(In(one))

       case  4 => 
         val one = argOne(at + 1)
         at += 2
         Some(Out(one))

       case  5 => 
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         at += 3
         Some(JumpTrue(one, two))

       case  6 =>
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         at += 3
         Some(JumpFalse(one, two))

       case  7 => 
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         val res = argRes(at + 3)
         at += 4
         Some(Less(one, two, res))

       case  8 =>
         val one = argOne(at + 1)
         val two = argTwo(at + 2)
         val res = argRes(at + 3)
         at += 4
         Some(Equal(one, two, res))

       case  9 =>
         val one = argOne(at + 1)
         at += 2
         Some(RelBase(one))

       case 99 => // halt
         at += 1
         Some(Halt)
         
       case  _ => // unknown
         err = true
         None
    }
  }

  def resolve(arg: Arg): Long = {
    arg match {
      case Val(x) => x
      case Mem(x) if x <= mem.size => mem(x)
      case Rel(x) if x <= mem.size => mem(rel + x)
      case _ => 
        err = true
        msg = s"Invalid arg: $arg"
        0
    }
  }

  def address(arg: Arg): Int = {
    arg match {
      case Mem(x) if (x >= 0) && (x < mem.size) => x
      case Rel(x) if (rel + x >= 0) && (rel + x < mem.size) => rel + x
      case _ => 
        err = true
        msg = s"Invalid output address: $arg"
        0
    }
  }

  def execute(op: Op): Unit = {
    op match {
      case Add(one, two, res) =>
        val addr = address(res)
        mem(addr) = resolve(one) + resolve(two)
      case Mul(one, two, res) =>
        val addr = address(res)
        mem(addr) = resolve(one) * resolve(two)
      case In(Mem(addr)) if (addr >= 0) && (addr < mem.size) =>
        if (in.nonEmpty) {
          mem(addr) = in.head
          in = in.tail
        } else {
          at -= 2
          blocked = true
        }
      case In(Rel(addr)) if (rel + addr >= 0) && (rel + addr < mem.size) =>
        if (in.nonEmpty) {
          mem(rel + addr) = in.head
          in = in.tail
        } else {
          at -= 2
          blocked = true
        }
      case Out(Mem(addr)) =>
        out += mem(addr)
      case Out(Rel(addr)) =>
        out += mem(rel + addr)
      case Out(Val(v)) =>
        out += v
      case JumpTrue(one, two) =>
        if (resolve(one) != 0) {
          at = resolve(two).toInt
        }
      case JumpFalse(one, two) =>
        if (resolve(one) == 0) {
          at = resolve(two).toInt
        }
      case Less(one, two, res) =>
        val addr = address(res)
        if (resolve(one) < resolve(two)) {
          mem(addr) = 1
        } else {
          mem(addr) = 0
        }
      case Equal(one, two, res) =>
        val addr = address(res)
        if (resolve(one) == resolve(two)) {
          mem(addr) = 1
        } else {
          mem(addr) = 0
        }
      case RelBase(one) =>
        rel += resolve(one).toInt
      case Halt => 
        halt = true
        msg = "OK"
      case _ =>
        err = true
        msg = s"Invalid op: $op"
    }
  }

  def step(): Unit = {
    fetch().foreach(op => {
      //println(f"$at%06d: $op")
      execute(op)
    })
  }

  def run(): Unit = {
    if (blocked) {
      blocked = in.isEmpty
    }
    while (!err && !halt && !blocked) {
      step()
    }
  }
}

