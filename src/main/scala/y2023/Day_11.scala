package com.cannondev.advent.y2023

import com.cannondev.advent.util.FileReader

import scala.io.Source

object Day_11:
  case class Position(line: Long, column: Long)

  val index = 11

  def parse(lines: List[String]): List[Position] =

    val numberPattern = "(#)".r

    val numbersPositions = for
      (line, index) <- lines.zipWithIndex
      number <- numberPattern.findAllMatchIn(line)
    yield Position(index, number.start)
    numbersPositions

  def compute(positions: List[Position]): Long =
    val distances = for {
      p1 <- positions
      p2 <- positions
    } yield (p1.line - p2.line).abs + (p1.column - p2.column).abs
    distances.sum / 2

  def expandSpace(positions: List[Position], times: Long, columnCount: Long, lineCount: Long): List[Position] =
    val emptyLines =
      (0.toLong until lineCount).foldLeft(List.empty[Long])((acc, el) => if (positions.exists(_.line == el)) acc else el :: acc)
    val emptyColums =
      (0.toLong until columnCount).foldLeft(List.empty[Long])((acc, el) =>
        if (positions.exists(_.column == el)) acc else el :: acc
      )
    val updatedLines = positions.map(p =>
      val emptyLinesCount = emptyLines.count(_ < p.line)
      p.copy(line = p.line + (emptyLinesCount) * (times - 1))
    )
    val updatedLinesAndColumns = updatedLines.map(p =>
      val emptyColumnsCount = emptyColums.count(_ < p.column)
      p.copy(column = p.column + (emptyColumnsCount) * (times - 1))
    )
    updatedLinesAndColumns

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val expanded = expandSpace(parsed,2, input.head.length, input.length)
    println(expanded)
    val expanded2 = expandSpace(parsed, 1000000, input.head.length, input.length)
    println(expanded2)
    println(compute(expanded))
    println(compute(expanded2))
