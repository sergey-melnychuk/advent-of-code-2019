import scala.io.Source

val line = Source.fromFile("input.txt").getLines.toList.head
val image = line.toCharArray

val Wide = 25
val Tall = 6

val layers = image.grouped(Wide * Tall).toList

val target = layers.map(layer => layer.groupBy(x => x).mapValues(_.size)).minBy(counts => counts.getOrElse('0', 0))

target('1') * target('2')
// 1560

// Part 2

def merge(pair: (Char, Char)): Char = pair match {
  case ('2', x) => x
  case (x, _) => x
}

val top = layers.reduce((hi, lo) => hi.zip(lo).map(merge))

val visible = top.map({
  case '2' | '1' => 'X'
  case '0' => ' '
})

visible.grouped(Wide).toList.map(_.mkString).foreach(println)

/*
X  X  XX   XX  X  X X  X
X  X X  X X  X X  X X  X
X  X X    X    X  X XXXX
X  X X XX X    X  X X  X
X  X X  X X  X X  X X  X
 XX   XXX  XX   XX  X  X
*/
// UGCUH


