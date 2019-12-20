import scala.io.Source

val lines = Source.fromFile("input.txt").getLines.toList

val pairs = lines.map(line => {
  val chunks = line.split("\\)")
  (chunks(0), chunks(1))
})

val parent = pairs.map(x => (x._2, x._1)).toMap

val tree = parent.toList.groupBy(x => x._2).mapValues(v => v.map(_._1))

case class Node(id: String, n: Long)

@annotation.tailrec
def bfs(queue: List[Node], seen: Set[String], acc: Map[String, Long] = Map.empty[String, Long]): Map[String, Long] = {
  if (queue.isEmpty) {
    acc
  } else {
    val marked = queue.map(_.id).toSet
    val next = queue.flatMap(node => 
      tree.getOrElse(node.id, Nil).map(id => 
        Node(id, node.n + 1)))

    bfs(next, seen ++ marked, acc ++ next.map(node => (node.id, node.n)).toMap)
  }
}

val root = Node("COM", 0)
val map = bfs(List(root), Set.empty[String])

map.values.sum
// 162439

// Part 2

@annotation.tailrec
def path(id: String, acc: List[String] = Nil): List[String] = {
  if (!parent.contains(id)) acc
  else {
    val p = parent(id)
    path(p, p :: acc)
  }
}

@annotation.tailrec
def prefix(one: List[String], two: List[String], len: Int = 0): Int = 
  (one, two) match {
    case (x :: xs, y :: ys) if x == y => prefix(xs, ys, len + 1)
    case _ => len
  }

val you = path("YOU")
val san = path("SAN")

val len = prefix(you, san)

(you.size - len) + (san.size - len)
// 367

