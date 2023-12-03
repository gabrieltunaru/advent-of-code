package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_3:
  case class Position(line: Int, start: Int, end: Int)
  case class NumberPosition(number: Int, line: Int, start: Int, end: Int)

  val index = 3

  def parse(lines: List[String]): (List[NumberPosition], List[Position]) =

    val numberPattern = "([0-9]+)".r
    val symbolPattern = "[!@#$%^&*()_+/=-]".r

    val numbersPositions = for
      (line, index) <- lines.zipWithIndex
      numbers <- numberPattern.findAllMatchIn(line)
    yield NumberPosition(numbers.matched.toInt, index, numbers.start, numbers.end)

    val symbolPositions = for
      (line, index) <- lines.zipWithIndex
      numbers <- symbolPattern.findAllMatchIn(line)
    yield Position(index, numbers.start, numbers.end)

    (numbersPositions, symbolPositions)

  def isAdiacent(number: NumberPosition, symbol: Position): Boolean =
    val isLineNear = (number.line - symbol.line).abs <= 1
    val isStartNear = (number.start - symbol.start).abs <= 1
    val isEndNear = (number.end - symbol.end).abs <= 1
    if (number.number == 592) {
//      println("debug")
    }
    isLineNear && (isStartNear || isEndNear)

  def part1(numberPositions: List[NumberPosition], symbolPositions: List[Position]): Int =
    val filtered = numberPositions.filter(number => symbolPositions.exists(symbol => isAdiacent(number, symbol)))
//    println(filtered)
    filtered.map(_.number).sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val (numbers, symbols) = parsed
    println(part1(numbers, symbols))
