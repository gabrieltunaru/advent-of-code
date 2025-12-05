package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_5:
  case class Input(intervals: List[Interval], values: List[BigInt])
  case class Interval(start: BigInt, stop: BigInt)

  val index = 5

  def parse(lines: String): Input =
    val split = lines.split("\n\n")
    val intervals = split.head
      .split("\n")
      .map {
        case s"$a-$b" => Interval(BigInt(a), BigInt(b))
        case s        => throw new Error(s"Invalid input for interval $s")
      }
    val values = split.tail.head.split("\n").map(x => BigInt(x))
    Input(intervals.toList, values.toList)

  def part1(input: Input): BigInt =
    input.values.count(v => input.intervals.exists(i => v >= i.start && v <= i.stop))

  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2025)
    val parsed = parse(input)
//    println(parsed)
    println(part1(parsed))
