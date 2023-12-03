import scala.io.Source

import day9.*

type BigRope = List[Point]

object Day9_2:
  val resources = "src/main/resources"
  val index = 9
  val filePath = s"$resources/day_$index.txt"

  def advancedPrintRope(rope: List[Point]) =
    println(rope)
    val xMin = rope.map(_.x).min
    val yMin = rope.map(_.y).min
    val xMax = rope.map(_.x).max + xMin.abs
    val yMax = rope.map(_.y).max + yMin.abs
    val matrix = Array.ofDim[String](xMax + xMin.abs + 1, yMax + yMin.abs + 1).map(_.map(_ => "-"))
    rope.zipWithIndex.foreach((p, i) => matrix(p.x + xMin.abs)(p.y + yMin.abs) = s"$i")
    matrix.transpose.reverse.foreach(l => println(l.mkString))
    println("")

  def minusOne(i: Int) =
    if (i == 0) 0
    else
      val one = i / i.abs
      i - one

  def moveTail(head: Point, tail: Point): Point =
    val xDiff = head.x - tail.x
    val yDiff = head.y - tail.y
    val xMove = minusOne(xDiff)
    val yMove = minusOne(yDiff)

    if (head==Point(2,2) && tail==Point(1,0))
      println("test")

    if (xDiff.abs == 2 && yDiff.abs == 2) Point(tail.x + xMove, tail.y + yMove)
    else if (xDiff.abs == 2 && yDiff.abs == 1) Point(tail.x + xMove, tail.y + yDiff)
    else if (xDiff.abs == 1 && yDiff.abs == 2) Point(tail.x + xDiff, tail.y + yMove)
    else if (xDiff.abs > 1) Point(tail.x + xMove, tail.y)
    else if (yDiff.abs > 1) Point(tail.x, tail.y + yMove)
    else tail

  def moveHead(rope: List[Point], x: Int, y: Int): List[Point] =
    val head = rope.head
    Point(head.x + x, head.y + y) :: rope.tail

  def step(rope: List[Point], x: Int, y: Int): List[Point] =
    val afterMovingHead = moveHead(rope, x, y)
    val head = afterMovingHead.head
    val afterMovingTail: List[Point] = afterMovingHead.tail
      .foldLeft(List(head))((acc, el) => moveTail(acc.head, el) :: acc)
    afterMovingTail.reverse

  def steps(rope: List[Point], x: Int, y: Int, amount: Int): List[List[Point]] =
    val res = (1 to amount).foldLeft(List(rope))((acc, _) => step(acc.head, x, y) :: acc)
    val head = res.head
//    res.reverse.foreach(l => advancedPrintRope(l))
    res

  def move(line: String, initial: List[Point]): List[List[Point]] =

    val up = "U (.*)".r
    val down = "D (.*)".r
    val left = "L (.*)".r
    val right = "R (.*)".r

    line match
      case up(v)    => steps(initial, 0, 1, v.toInt)
      case down(v)  => steps(initial, 0, -1, v.toInt)
      case left(v)  => steps(initial, -1, 0, v.toInt)
      case right(v) => steps(initial, 1, 0, v.toInt)
      case e        => throw new Error(s"invalid input: $e")

  def compute(lines: List[String]): List[List[Point]] =
    def rope = List.fill(10)(Point(0, 0))
//    lines.foldLeft(List(rope))((ropes, line) => move(line, ropes.head).appendedAll(ropes))
    lines.foldLeft(List(rope))((ropes, line) =>
      val res = move(line, ropes.head) ++ ropes
//      println(res)
      res
    )

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines().toList

    val res = compute(fileContents)
//    val tails = res.map(_.tail)
//    val distinctTails = tails.flatten.distinct
//    tails.foreach(tail => println(s"(${tail.x}, ${tail.y})"))
//    res.foreach(println)
    val distinctTails = res.map(_(9)).distinct
    distinctTails.foreach(println)
    println(distinctTails.size)
