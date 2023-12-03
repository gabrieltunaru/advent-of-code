import scala.io.Source

import day9.*
object Day9_1:
  val resources = "src/main/resources"
  val index = 9
  val filePath = s"$resources/day_$index.txt"

  def printRope(rope: Rope) =
    val xmax = 6
    val ymax = 5
    val matrix = Array.ofDim[String](xmax, ymax).map(_.map(_ => "-"))
    matrix(rope.head.x)(rope.head.y) = "H"
    matrix(rope.tail.x)(rope.tail.y) = "T"
    matrix.transpose.reverse.foreach(l => println(l.mkString))
    println("")

  def minusOne(i: Int) =
    if (i==0) 0
    else
      val one = i/i.abs
      i - one

  def moveTail(rope: Rope): Rope =
    val xDiff = rope.head.x - rope.tail.x
    val yDiff = rope.head.y - rope.tail.y
    val xMove = minusOne(xDiff)
    val yMove = minusOne(yDiff)
    val tail = rope.tail

    if (xDiff.abs == 2 && yDiff.abs == 1) rope.copy(tail = Point(tail.x + xMove, tail.y + yDiff))
    else if (xDiff.abs == 1 && yDiff.abs == 2) rope.copy(tail = Point(tail.x + xDiff, tail.y + yMove))
    else if (xDiff.abs > 1)  rope.copy(tail = Point(tail.x + xMove, tail.y))
    else if (yDiff.abs > 1)  rope.copy(tail = Point(tail.x, tail.y + yMove))
    else rope

  def moveHead(rope: Rope, x: Int, y: Int): Rope =
    val head = rope.head
    rope.copy(head = Point(head.x + x, head.y + y))

  def step(rope: Rope, x: Int, y: Int): Rope =
//    println(s"$rope, $x, $y")
    val afterMovingHead = moveHead(rope, x, y)
//    printRope(afterMovingHead)
    val afterMovingTail = moveTail(afterMovingHead)
//    printRope(afterMovingTail)
    afterMovingTail

  def steps(rope: Rope, x: Int, y: Int, amount: Int): List[Rope] =
    (1 to amount).foldLeft(List(rope))((acc, _) => step(acc.head, x, y) :: acc)

  def move(line: String, initial: Rope): List[Rope] =

    val up = "U (.*)".r
    val down = "D (.*)".r
    val left = "L (.*)".r
    val right = "R (.*)".r

//    println(line)

    line match
      case up(v)    => steps(initial, 0, 1, v.toInt)
      case down(v)  => steps(initial, 0, -1, v.toInt)
      case left(v)  => steps(initial, -1, 0, v.toInt)
      case right(v) => steps(initial, 1, 0, v.toInt)
      case e        => throw new Error(s"invalid input: $e")

  def compute(lines: List[String]): List[Rope] =
    def head = Point(0, 0)
    def tail = Point(0, 0)
    def rope = Rope(head, tail)
//    lines.foldLeft(List(rope))((ropes, line) => move(line, ropes.head).appendedAll(ropes))
    lines.foldLeft(List(rope))((ropes, line) =>
      val res = move(line, ropes.head) ++ ropes
//      println(res)
      res
    )

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines().toList

    val res = compute(fileContents)
    val tails = res.map(_.tail)
    val distinctTails = tails.distinct
//    tails.foreach(tail => println(s"(${tail.x}, ${tail.y})"))
//    res.foreach(println)
    println(distinctTails.size)
