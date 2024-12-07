package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_4:

  val index = 4

  private val nextCharMap =
    Map(('X' -> (Some('M'), false)), ('M' -> (Some('A'), false)), ('A' -> (Some('S'), false)), ('S' -> (None, true)))

  def parse(lines: List[String]): List[List[Char]] =
    lines.map(_.toCharArray.toList)

  def calculateFromStartingPoint(map: List[List[Char]], x: Int, y: Int, visited: List[(Int, Int)]): Int =
    val xMax = map.length
    val yMax = map.head.length
    val currentChar = map(x)(y)
    // TODO: if current char is X, reset visited!
    // TODO: only go to next position if next char is there, else reset
    // TODO: diagonally means only one direction, not all diagonals at the same time
    val (nextChar, found) = nextCharMap.getOrElse(currentChar, (None, false))
    val actualVisited = if (currentChar == 'X') List.empty else visited
    if (found) 1
    else
      val nextIndexes = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, -1), (-1, 1), (1, 1), (-1, -1))
      val nextPositions =
        nextIndexes.map((a, b) => (x + a, y + b))
      val nextPositionsFiltered =
        nextPositions.filter((a, b) => a >= 0 && b >= 0 && a < xMax && b < yMax && !actualVisited.contains((a, b)))
      nextPositionsFiltered
        .map((a, b) =>
          if (nextChar.contains(map(a)(b))) calculateFromStartingPoint(map, a, b, (x, y) :: actualVisited)
          else 0
        )
        .sum

  def part1(map: List[List[Char]]): Int = {
    val res = for {
      i <- map.indices
      j <- map(i).indices
      result = if (map(i)(j) == 'X') calculateFromStartingPoint(map, i, j, List.empty) else 0
    } yield result
    println(res.grouped(map.head.length).foreach(println))
    res.sum
  }

  def findAllAdiacent(map: List[List[Char]], x: Int, y: Int): Int =
    val nextIndexes = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, -1), (-1, 1), (1, 1), (-1, -1))
    val xMax = map.length
    val yMax = map.head.length
    val count = for {
      currentDirection <- nextIndexes
      wordIndices = (0 to 3).map(i => (x + currentDirection._1 * i, y + currentDirection._2 * i))
      filteredIndices = wordIndices.filter((a, b) => a >= 0 && b >= 0 && a < xMax && b < yMax)
      actualWord = filteredIndices.map((a, b) => map(a)(b))
    } yield if (actualWord.mkString == "XMAS") 1 else 0
    count.sum

  def part1Simple(map: List[List[Char]]): Int = {
    val res = for {
      i <- map.indices
      j <- map(i).indices
      result = findAllAdiacent(map, i, j)
    } yield result
    res.sum
  }

  def findAllAdiacent2Mas(map: List[List[Char]], x: Int, y: Int): Int =
    val nextIndexes = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, -1), (-1, 1), (1, 1), (-1, -1))
    val upLeft = (x - 1, y - 1)
    val downLeft = (x + 1, y - 1)
    val upRight = (x - 1, y + 1)
    val downRight = (x + 1, y + 1)
    val xMax = map.length
    val yMax = map.head.length
    val isWhitinBounds =
      (upLeft :: upRight :: downLeft :: downRight :: Nil).forall((a, b) => a >= 0 && b >= 0 && a < xMax && b < yMax)

    if (isWhitinBounds)

      val firstMas = "" + map(upLeft._1)(upLeft._2) + map(downRight._1)(downRight._2)
      val secondMas = "" + map(upRight._1)(upRight._2) + map(downLeft._1)(downLeft._2)
      if (
        map(x)(y) == 'A' &&
        (firstMas == "MS" || firstMas == "SM") &&
        (secondMas == "MS" || secondMas == "SM")
      ) 1
      else 0
    else 0

  def part2Simple(map: List[List[Char]]): Int = {
    val res = for {
      i <- map.indices
      j <- map(i).indices
      result = findAllAdiacent2Mas(map, i, j)
    } yield result
    res.sum
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)

    println(part1Simple(parsed))
    println(part2Simple(parsed))
