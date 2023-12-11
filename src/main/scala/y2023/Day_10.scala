package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_10:
  case class Pipe(pipeType: Char, line: Int, column: Int)

  val index = 10

  def getConnectingPipes(pipes: List[Pipe], current: Pipe) =
    val theoreticConnectingPipes = current.pipeType match
      case '|' => List(1,-1).map(x => current.copy(line = current.line + x))
      case '-' => List(1,-1).map(x => current.copy(column =  current.column + x))

  def parse(lines: List[String]): List[Pipe] =
    lines.zipWithIndex.flatMap((line, y) =>
      line.toCharArray.zipWithIndex.filter((c, _) => c != '.').map((c, x) => Pipe(c, x, y)).toList
    )

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
