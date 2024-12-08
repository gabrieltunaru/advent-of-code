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

  def substract(input: Input, diffX: Int, diffY: Int, startX: Int, startY: Int): Set[Antinode] =
    val antinode = Antinode(startX - diffX, startY - diffY)
    if (antinode.x < 0 || antinode.y < 0 || antinode.x >= input.xMax || antinode.y >= input.yMax) Set.empty
    else substract(input, diffX, diffY, antinode.x, antinode.y) + antinode

  def add(input: Input, diffX: Int, diffY: Int, startX: Int, startY: Int): Set[Antinode] =
    val antinode = Antinode(startX + diffX, startY + diffY)
    if (antinode.x < 0 || antinode.y < 0 || antinode.x >= input.xMax || antinode.y >= input.yMax) Set.empty
    else add(input, diffX, diffY, antinode.x, antinode.y) + antinode

  def debug(input: Input, antinodes: Set[Antinode]) =
    val chars = for {
      i <- 0 to input.xMax
      j <- 0 to input.yMax
      maybeAntena = input.antennas.find(a => a.x == i && a.y == j)
      maybeAntinode = antinodes.find(a => a.x == i && a.y == j)
    } yield
      if (maybeAntena.isDefined) maybeAntena.get.code
      else if (maybeAntinode.isDefined) "#"
      else "."
    chars.grouped(input.xMax + 1).map(_.mkString).foreach(println)

  def part2(input: Input): Int =
    val antinodes = for {
      a1 <- input.antennas
      a2 <- input.antennas.filter(_.code == a1.code) - a1
      diffX = a2.x - a1.x
      diffY = a2.y - a1.y
    } yield substract(input, diffX, diffY, a1.x, a1.y) ++ add(input, diffX, diffY, a2.x, a2.y)
    val notSingleAntennasCodes = input.antennas.filter(a => input.antennas.count(_.code == a.code) > 1).map(_.code)
    val notSingleAntennas = input.antennas
      .filter(a => notSingleAntennasCodes.contains(a.code))
      .filter(antenna => !antinodes.flatten.exists(antinode => antinode.x == antenna.x && antinode.y == antenna.y))
//    debug(input, antinodes.flatten)
    antinodes.flatten.count(a => a.x >= 0 && a.x < input.xMax && a.y >= 0 && a.y < input.yMax) + notSingleAntennas.size

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed)
//    println(part1(parsed))
    println(part1(parsed).size)
    println(part2(parsed))
