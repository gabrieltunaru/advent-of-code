package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_1:
  case class Something(l1: List[Int], l2: List[Int])

  val index = 1

  def parse(lines: List[String]): Something =
    val filtered = lines
      .map(_.split("   "))
      .map(a => (a(0).toInt, a(1).toInt))
      .unzip
//    println(filtered)

    Something(filtered._1, filtered._2)

  def part1(input: Something): BigInt =
    val l1 = input.l1.sorted
    val l2 = input.l2.sorted
    val diffs = l1.zip(l2).map { case (a, b) => a - b }.map(_.abs)
    val sum2 = diffs.foldLeft(BigInt(0))(_ + _)
    sum2

  def part2(input: Something): BigInt =
    val products = input.l1.map(x => x * input.l2.count(y => x == y))
    val sum = products.foldLeft(BigInt(0))(_ + _)
    sum
  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(part1(parsed))
    println(part2(parsed))

/** Tried values 1151792
  */
