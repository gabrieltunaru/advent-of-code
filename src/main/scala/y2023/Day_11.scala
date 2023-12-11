package com.cannondev.advent.y2023

import com.cannondev.advent.util.FileReader

import scala.io.Source

object Day_11:
  case class Position(line: Int, column: Int)

  val index = 11

  def parse(lines: List[String]): List[Position] =

    val numberPattern = "(#)".r

    val numbersPositions = for
      (line, index) <- lines.zipWithIndex
      number <- numberPattern.findAllMatchIn(line)
    yield Position(index, number.start)
    numbersPositions

  def expandSpace(positions: List[Position]): List[Position] =
    // gaseste toate liniile goale
    // gaseste toate coloanele goale
    // parcurgi listele astea 2 si adaugi spatiu
    val emptyLines =
      positions.indices.foldLeft(List.empty[Int])((acc, el) => if (positions.exists(_.line == el)) acc else el :: acc)
    val emptyColums =
      positions.indices.foldLeft(List.empty[Int])((acc, el) => if (positions.exists(_.column == el)) acc else el :: acc)
    val updatedLines = positions.map(p => p.copy(line = p.line + emptyLines.count(_ < p.line)))
    val updatedLinesAndColumns = updatedLines.map(p => p.copy(column = p.column + emptyColums.count(_ < p.column)))
    updatedLinesAndColumns

  def part1(positions: List[Position]): Int =
    val distances = for {
      p1 <- positions
      p2 <- positions
    } yield (p1.line - p2.line).abs + (p1.column - p2.column).abs
    distances.sum / 2

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val expanded = expandSpace(parsed)
    println(expanded)
    println(part1(expanded))
