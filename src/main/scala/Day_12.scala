package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_12:
  case class Point(x: Int, y: Int)
  type Something = List[List[Int]]

  val index = 12

  def getElevation(point: Point, matrix: List[List[Char]]): Char =
    matrix(point.x)(point.y) match
      case 'S' => 'a'
      case 'E' => 'z'
      case c   => c

  def isValid(curr: Point, next: Point, matrix: List[List[Char]]): Boolean =
    val currElevation = getElevation(curr, matrix)
    val nextElevation = getElevation(next, matrix)
    val heightDiff = nextElevation - currElevation
    heightDiff == 0 || heightDiff == 1

  def backtracking(
      curr: Point,
      matrix: List[List[Char]],
      result: List[Char],
      visited: List[List[Boolean]],
      allResults: List[List[Char]]
  ): List[List[Char]] =
    val x = curr.x
    val y = curr.y
    if (visited(x)(y)) return Nil
    if (matrix(x)(y) == 'E') return result.appended('E') :: allResults
    println(curr)

    // speedup
    val maybeMin = allResults.map(_.size).minOption
    if (maybeMin.isDefined)
      if (result.size >= allResults.map(_.size).min) return Nil

//    debugging.foreach(l => println(l.mkString))
//    println("")
    val nextResult = result.appended(matrix(x)(y))
    val nextVisited = visited.updated(x, visited(x).updated(y, true))
    val nextPoints = Point(x + 1, y) :: Point(x, y + 1) :: Point(x - 1, y) :: Point(x, y - 1) :: Nil
    val filteredNextPoints = nextPoints
      .filter(p => p.x >= 0 && p.y >= 0 && p.x < matrix.size && p.y < matrix.head.size)
      .filter(p => !visited(p.x)(p.y))
      .filter(p => isValid(curr, p, matrix))
//    if (x == 2) {
//      println("test")
//    }
    val nextResults = filteredNextPoints.map(p => backtracking(p, matrix, nextResult, nextVisited, allResults))

    nextResults.flatten ++ allResults

  def parse(lines: List[String]): List[List[Char]] =
    lines.foldLeft(List.empty[List[Char]])((acc, el) => acc.appended(el.toCharArray.toList))

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val parsed = parse(input)
    val startX = parsed.indexWhere(_.contains('S'))
    val startY = parsed(startX).indexOf('S')
    val start = Point(startX, startY)
    val solutions = backtracking(
      start,
      parsed,
      List.empty,
      parsed.map(_ => List.fill(parsed.head.size)(false)),
      List.empty
    )
    val min = solutions.map(_.size).min
//    solutions.foreach(s => println(s"${s.size}: $s"))
//    println(solutions.map(_.size))
//    println(solutions.find(_.size==min))
    println(min)
