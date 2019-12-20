// Advent of Code 2019

sealed trait Arg
case class Val(value: Int) extends Arg
case class Mem(addr: Int) extends Arg

sealed trait Op
case class Add(one: Arg, two: Arg, res: Mem) extends Op
case class Mul(one: Arg, two: Arg, res: Mem) extends Op

case class In(addr: Int) extends Op
case class Out(addr: Int) extends Op

case class JumpTrue(one: Arg, two: Arg) extends Op
case class JumpFalse(one: Arg, two: Arg) extends Op
case class Less(one: Arg, two: Arg, res: Mem) extends Op
case class Equal(one: Arg, two: Arg, res: Mem) extends Op

case object Halt extends Op

class Intcode(program: List[Int], var io: Int = 0) {
  val mem = program.toArray
  var at: Int = 0
  var err: Boolean = false
  var halt: Boolean = false

  def fetch(): Option[Op] = {
    val code = mem(at) % 100
    val opts = mem(at) / 100
    val (a, b, c) = (0, (opts / 10) % 10, opts % 10)
    val argOne: Int => Arg = (cell: Int) => if (c > 0) Val(mem(cell)) else Mem(mem(cell))
    val argTwo: Int => Arg = (cell: Int) => if (b > 0) Val(mem(cell)) else Mem(mem(cell))
    val argRes: Int => Mem = (cell: Int) => Mem(mem(cell))
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
         val res = mem(at + 1)
         at += 2
         Some(In(res))

       case  4 => 
         val res = mem(at + 1)
         at += 2
         Some(Out(res))

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

       case 99 => // halt
         at += 1
         Some(Halt)
         
       case  _ => // unknown
         err = true
         None
    }
  }

  def resolve(arg: Arg): Int = {
    arg match {
      case Val(x) => x
      case Mem(x) => mem(x)
    }
  }

  def execute(op: Op): Unit = {
    op match {
      case Add(one, two, res) =>
        mem(res.addr) = resolve(one) + resolve(two)
      case Mul(one, two, res) =>
        mem(res.addr) = resolve(one) * resolve(two)
      case In(addr) =>
        mem(addr) = io 
      case Out(addr) =>
        io = mem(addr)
      case JumpTrue(one, two) =>
        if (resolve(one) != 0) {
          at = resolve(two)
        }
      case JumpFalse(one, two) =>
        if (resolve(one) == 0) {
          at = resolve(two)
        }
      case Less(one, two, res) =>
        if (resolve(one) < resolve(two)) {
          mem(res.addr) = 1
        } else {
          mem(res.addr) = 0
        }
      case Equal(one, two, res) =>
        if (resolve(one) == resolve(two)) {
          mem(res.addr) = 1
        } else {
          mem(res.addr) = 0
        }
      case Halt => 
        halt = true
    }
  }

  def step(): Unit = {
    fetch().foreach(execute)
  }

  def run(): Unit = {
    while (!err && !halt) {
      step()
    }
  }
}

