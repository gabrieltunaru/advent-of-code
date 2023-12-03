package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_3:
  case class SymbolPosition(symbol: String, line: Int, start: Int, end: Int)
  case class NumberPosition(number: Int, line: Int, start: Int, end: Int)

  val index = 3

  def parse(lines: List[String]): (List[NumberPosition], List[SymbolPosition]) =

    val numberPattern = "([0-9]+)".r
    val symbolPattern = "[!@#$%^&*()_+/=-]".r

    val numbersPositions = for
      (line, index) <- lines.zipWithIndex
      number <- numberPattern.findAllMatchIn(line)
    yield NumberPosition(number.matched.toInt, index, number.start, number.end)

    val symbolPositions = for
      (line, index) <- lines.zipWithIndex
      symbol <- symbolPattern.findAllMatchIn(line)
    yield SymbolPosition(symbol.matched, index, symbol.start, symbol.end)

    (numbersPositions, symbolPositions)

  def isAdiacent(number: NumberPosition, symbol: SymbolPosition): Boolean =
    val isLineNear = (number.line - symbol.line).abs <= 1
    val isStartNear = (number.start - symbol.start).abs <= 1
    val isEndNear = (number.end - symbol.end).abs <= 1
    isLineNear && (isStartNear || isEndNear)

  def part1(numberPositions: List[NumberPosition], symbolPositions: List[SymbolPosition]): Int =
    val filtered = numberPositions.filter(number => symbolPositions.exists(symbol => isAdiacent(number, symbol)))
    filtered.map(_.number).sum

  def part2(numberPositions: List[NumberPosition], symbolPositions: List[SymbolPosition]): Int =
    val starSymbols = symbolPositions.filter(_.symbol == "*")
    val validGears = starSymbols.filter(starSymbol => {
      val adiacentNumbers = numberPositions.count(number => isAdiacent(number, starSymbol))
      adiacentNumbers == 2
    })
    val ratios =
      validGears.map(gear => numberPositions.filter(number => isAdiacent(number, gear)).map(_.number).product)
    ratios.sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val (numbers, symbols) = parsed
    println(part1(numbers, symbols))
    println(part2(numbers, symbols))
