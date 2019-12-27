import scala.io.Source

case class Part(name: String, amount: Long)

case class Formula(from: List[Part], to: Part)

def fetchPart(str: String): Part = {
  val Array(amount, name) = str.split(" ")
  Part(name, amount.toLong)
}

def fetchFormula(line: String): Formula = {
  val Array(lhs, rhs) = line.split(" => ")
  val to = fetchPart(rhs)
  val from = lhs.split(", ").toList.map(fetchPart)
  Formula(from, to)
}

def parseInput(file: String): List[Formula] = {
  Source.fromFile(file).getLines.toList.map(fetchFormula)
}

def buildIndex(input: List[Formula], exclude: Set[String] = Set.empty[String]): Map[String, Formula] = {
  input.filterNot(f => (f.from.size == 1) && exclude(f.from.head.name)).map(f => (f.to.name, f)).toMap
}

class Reactor(index: Map[String, Formula]) {
  val available = scala.collection.mutable.Map.empty[String, Long]
  val remaining = scala.collection.mutable.Map.empty[String, Long]
  val required = scala.collection.mutable.Map.empty[String, Long]

  def react(target: Part): Unit = {
    //println(s"react($target)")
    if (remaining.getOrElse(target.name, 0L) >= target.amount) {
      // required element can be taken from residuals
      val rest = remaining(target.name) - target.amount
      available += (target.name -> target.amount)
      if (rest > 0) {
        remaining += (target.name -> rest)
      } else {
        remaining -= target.name
      }
      required -= target.name
    } else if (index.contains(target.name)) {
      // required element needs reaction to be performed
      val formula = index(target.name)
      val k = math.ceil(target.amount.toDouble / formula.to.amount).toInt
      val produced = k * formula.to.amount
      available += (target.name -> target.amount)
      val residude = produced - target.amount
      if (residude > 0) {
        val current = remaining.getOrElse(target.name, 0L)
        remaining += (target.name -> (residude + current))
      }

      // now all of `formula.from` parts are required (with `k` multiplicator)
      formula.from.foreach(part => {
        val needed = k * part.amount
        if (remaining.getOrElse(part.name, 0L) == 0) {
          val current = required.getOrElse(part.name, 0L)
          required += (part.name -> (needed + current))
        } else {
          val current = remaining(part.name)
          if (current == needed) {
            remaining -= part.name
          } else if (current > needed) {
            available += (part.name -> needed)
            remaining += (part.name -> (current - needed))
          } else { // current < needed
            remaining -= part.name
            required += (part.name -> (required.getOrElse(part.name, 0L) + needed - current))
          }
        }
      })

      required -= target.name
    }
  }

  def resolve(target: Part): Map[String, Long] = {
    react(target)
    var previous = Set.empty[String]
    while (!(required.keySet -- previous).isEmpty) {
      println
      println("required:\n" + required.mkString("\n"))
      println("available:\n" + available.mkString("\n"))
      println("remaining:\n" + remaining.mkString("\n"))
      val current = required.clone()
      previous = Set(current.keySet.toList: _*)
      remaining.clear()
      current.toList.map({ case (k, v) => Part(k, v) }).foreach(react)
    }
    Map(required.toList: _*)
  }
}

def trace(index: Map[String, Formula], part: Part): Part = {
  println(s"trace($part)")
  
  @annotation.tailrec
  def sub(required: Map[String, Long]): Map[String, Long] = {
    val next = required.toList
      .flatMap({ case (name, amount) =>
        index.get(name) match {
          case None => Nil
          case Some(formula) => 
            val k = math.ceil(amount.toDouble / formula.to.amount).toLong
            formula.from
              .map(p => Part(p.name, k * p.amount))
              .map(p => trace(index, p))
              .map(p => (p.name, p.amount))
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)

    if (next.isEmpty) required
    else sub(next)
  }

  sub(Map(part.name -> part.amount))
    .toList
    .groupBy(_._1)
    .mapValues(_.map(_._2).sum)
    .map({ case (k, v) => Part(k, v) })
    .head
}

def part1(file: String): Long = {
  val input = parseInput(file)
  val reactor = new Reactor(buildIndex(input, Set("ORE")))
  val target = reactor.resolve(Part("FUEL", 1))
  target.foreach(println)
  
  val index = buildIndex(input)
  val out = target.toList.map({ case (name, amount) =>
    val formula = index(name)
    val ore = formula.from.head.amount
    val k = math.ceil(amount.toDouble / formula.to.amount).toInt
    ore * k
  })
  println(out.sum)

  val v2 = target.toList.map({ case (k, v) => Part(k, v) }).map(p => trace(index, p).amount).sum
  println(v2)

  val v3 = trace(index, Part("FUEL", 1))
  println(v3.amount)
  0
}


class Reactor2(index: Map[String, Formula]) {
  val remaining = scala.collection.mutable.Map.empty[String, Long]

  def reset(): Unit = {
    remaining.clear();
  }

  def ore(name: String, amount: Long): Long = {
    if (name == "ORE") amount
    else {
      val exists = remaining.getOrElse(name, 0L)
      if (amount <= exists) {
        val rest = exists - amount
	if (rest > 0) {
	  remaining += (name -> rest)
        } else {
          remaining -= name
        }
        0
      } else {
        val formula = index(name)
        val target = amount - exists
        val k = math.ceil(target.toDouble / formula.to.amount).toInt
        val produced = k * formula.to.amount

        remaining -= name
        val out = formula.from.map(p => ore(p.name, k * p.amount)).sum

        val residude = produced - target
        if (residude > 0) {
          val rest = residude + remaining.getOrElse(name, 0L)
          remaining += (name -> rest)
        }

        out
      }
    }
  }
}

def p1(file: String): Long = {
  val input = parseInput(file)
  val index = buildIndex(input)
  val reactor = new Reactor2(index)
  reactor.ore("FUEL", 1)
}

p1("input.txt")
// 136771

def p2(file: String): Long = {
  val index = buildIndex(parseInput(file))

  val max = 1000000000000L
  val hi = max
  val lo = 1L

  var step: Long = hi - lo
  var fuel: Long = 1L 
  var ore: Long = 0L

  var target: Long = 1L

  val reactor = new Reactor2(index)
  while (step > 0) {
    reactor.reset()
    ore = reactor.ore("FUEL", fuel)
    //println(s"ORE: $ore, FUEL: $fuel, step: $step")

    step = step >> 1

    if (ore > max) {
      fuel -= step
    } else {
      //println(s"\ttarget: $target, fuel: $fuel, ore: $ore")
      target = target max fuel
      fuel += step
    }
  }

  val N = 10000

  var result: Long = target
  var x: Long = target - N
  while (x <= target + N) {
    reactor.reset()
    ore = reactor.ore("FUEL", x)
    if (ore <= max) {
      result = result max x
    }
    x += 1
  }
  result
}

p2("input.txt")
// 8193614

