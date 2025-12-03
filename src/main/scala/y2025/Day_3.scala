package com.cannondev.advent
package y2025
import util.FileReader

import scala.io.Source

object Day_3:
  case class Something(value: Int)

  val index = 3

  def findMax(c: List[Char], digits: Int): BigInt = {
    val withIndex = c.zipWithIndex
    //
    val secondMax = (0 until digits).foldLeft(List.empty[(Char, Int)])((acc, el) => {
      val start = acc.map(_._2).headOption.getOrElse(-1)
      val remainingDigits = digits - acc.length
      val stop = c.length - remainingDigits
      val filtered = withIndex.filter((_, i) => i > start && i <= stop)
      val nextMax = filtered.maxBy(_._1)
      nextMax :: acc
    })
    val result = BigInt(secondMax.sortBy(_._2).map(_._1).mkString)
    result
  }

  def part1(input: List[Array[Char]]): BigInt = {
    input.map(x => findMax(x.toList, 2)).sum
  }

  def part2(input: List[Array[Char]]): BigInt = {
    input.map(x => findMax(x.toList, 12)).sum
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = input.map(_.toCharArray)
    println(parsed)
    println(part1(parsed))
    println(part2(parsed))
