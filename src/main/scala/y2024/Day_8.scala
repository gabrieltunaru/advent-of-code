package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_8:
  case class Antenna(x: Int, y: Int, code: Char)
  case class Antinode(x: Int, y: Int)

  case class Input(xMax: Int, yMax: Int, antennas: Set[Antenna])

  val index = 8

  def parse(lines: List[String]): Input =

    val pattern = "([a-zA-Z0-9])".r

    val parsedMap = for
      i <- lines.indices
      j <- lines.head.indices
      parsed = lines(i)(j) match
        case pattern(c) => Some(Antenna(i, j, c))
        case _          => None
    yield parsed
    Input(lines.length, lines.head.length, parsedMap.flatten.toSet)

  def part1(input: Input): Set[Antinode] =
    val antinodes = for {
      a1 <- input.antennas
      a2 <- input.antennas.filter(_.code == a1.code) - a1
      diffX = a2.x - a1.x
      diffY = a2.y - a1.y
    } yield List(Antinode(a1.x - diffX, a1.y - diffY), Antinode(a2.x + diffX, a2.y + diffY))
    antinodes.flatten.filter(a => a.x >= 0 && a.x < input.xMax && a.y >= 0 && a.y < input.yMax)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed)
//    println(part1(parsed))
    println(part1(parsed).size)
