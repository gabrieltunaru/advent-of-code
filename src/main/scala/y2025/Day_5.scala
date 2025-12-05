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

  def union(a: Interval, b: Interval): BigInt =
    val start = a.start.max(b.start)
    val stop = a.stop.min(b.stop)
    if (stop >= start) stop - start + 1
    else 0

  def sot(intervals : List[Interval]): List[Interval] = {
    val sorted = intervals.sortBy(i => i.start)
    sorted.sliding(2).
  }

  def part2(intervals: List[Interval]): BigInt =
    val total = intervals.map(i => i.stop - i.start + 1).sum
    val edges = intervals.flatMap(i => List(i.start, i.stop))
    val edgesSet = edges.toSet
    val edgesCount = edgesSet.map(edge => edges.count(e => e == edge))
    val overlaps = for {
      i <- intervals
      j <- intervals
    } yield if (i != j) union(i, j) else BigInt(0)
    val toSubtractDuplicates = overlaps.sum
    total - toSubtractDuplicates

  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2025)
    val parsed = parse(input)
//    println(parsed)
    println(part1(parsed))
    println(part2(parsed.intervals))
