package com.cannondev.advent
package y2025

import util.FileReader


object Day_2:
  case class Interval(start: BigInt, stop: BigInt)

  val index = 2

  // 123120
  // 123143
  // diff: 23
//  def findDuplications(interval: Interval): List[BigInt] = {
//    val commonDigitsS = (interval.start.toString.toCharArray.zipAll(interval.stop.toString.toCharArray, '*', '#').map {
//      case (a, b) if (a == b) => s"$a$b"
//      case _ => ""
//    }.mkString
//  val commonDigits = BigInt(commonDigitsS)
//    ???
//
//  }

  def isRepeating(x: BigInt): Boolean =
    val (firstHalf, secondHalf) = x.toString.splitAt(x.toString.length/2)
    firstHalf == secondHalf

  def expandInterval(interval: Interval): List[BigInt] =
    interval.start.to(interval.stop).toList

  def part1(intervals: List[Interval]): BigInt =
    val expanded = intervals.flatMap(expandInterval)
    val duplicated = expanded.filter(isRepeating)
    duplicated.sum

  def parse(lines: String): List[Interval] =
    val intervalsString = lines.split(",")
    intervalsString.map {
      case s"$a-$b" => Interval(BigInt(a), BigInt(b))
      case s => throw new Error(s"wrong interval format $s")
    }.toList

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input.head)
    println(parsed)
    println(part1(parsed))
