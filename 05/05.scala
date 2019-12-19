
sealed trait Arg
case class Val(value: Int) extends Arg
case class Mem(index: Int) extends Arg

sealed trait Op
case class Add(one: Arg, two: Arg, res: Mem) extends Op
case class Mul(one: Arg, two: Arg, res: Mem) extends Op

case class In(addr: Mem) extends Op
case class Out(addr: Mem) extemds Op

case object Halt extends Op

class Intcode(codes: List[Int], var io: Int = 0) {
  val mem = program.toArray
  var at: Int = 0
  var err: Boolean = false
  var halt: Boolean = false

  def fetch(): Option[Op] = {
    val code = mem(at)
    val opts = code / 100
    val (a, b, c) = (0, (opts / 10) % 10, opts % 10)
    val argOne: Arg = (x: Int) => if (c > 0) Val(x) else Mem(x)
    val argTwo: Arg = (x: Int) => if (b > 0) Val(x) else Mem(x)
    val argRes: Mem = (x: Int) => Mem(x)
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
         val res = argRes(at + 1)
         at += 2
         Some(In(res))

       case  4 => 
         val res = argRes(at + 1)
         at += 2
         Some(Out(res))

       case 99 => // halt
         at += 1
         Some(Halt)
         
       case  _ => // unknown
         error = true
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
      case In(res) =>
        mem(res.addr) = io 
      case Out(res) =>
        io = mem(res.addr)
      case Halt => 
        halt = true
    }
  }

  def step(): Unit = {
    fetch().foreach(execute)
  }

  def run(): Unit = {
    while (!error && !halt) {
      step()
    }
  }
}

