package com.cannondev.advent
package y2024

import com.cannondev.advent.util.FileReader

import scala.io.Source

object Day_3:
  case class Something(x: Int, y: Int)

  val index = 3

  /** tried: 182780583
    */
  def parse(lines: List[String]): List[Something] =

    val pattern = "mul\\(([0-9]+),([0-9]+)\\)".r
    val input = lines.mkString
    pattern
      .findAllIn(input)
      .map {
        case pattern(x, y) => Something(x.toInt, y.toInt)
        case _             => throw new Error("Impossible")
      }
      .toList

  def part1(input: List[Something]): BigInt =
    input.map(a => a.x * a.y).foldLeft(BigInt(0))((a, b) => a + b)

  def parsePart2(lines: List[String]): List[String] =

    val pattern = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)".r
    val input = lines.mkString
    pattern
      .findAllIn(input)
      .toList

  def filter(input: List[String], enabled: Boolean): List[String] =
    input match
      case head :: tail if head == "don't()" => filter(tail, false)
      case head :: tail if head == "do()"    => filter(tail, true)
      case _ :: tail if !enabled             => filter(tail, enabled)
      case head :: tail                      => head :: filter(tail, enabled)
      case Nil                               => Nil

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
//    println(parsed)
    println(part1(parsed))
    println(parsePart2(input))
    println(part1(parse(filter(parsePart2(input), true))))
