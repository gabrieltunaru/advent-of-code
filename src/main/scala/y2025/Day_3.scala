package com.cannondev.advent
package y2025
import util.FileReader

import scala.io.Source

object Day_3:
  case class Something(value: Int)

  val index = 3

  def findMax2(c: Array[Char]): Int = {
    val withIndex = c.zipWithIndex
    val max = withIndex.reverse.tail.reverse.maxBy(_._1)
    val secondMax = withIndex.filterNot((_, i) => i <= max._2 ).maxBy(_._1)
    val result = List(max, secondMax).sortBy(_._2).map(_._1).mkString.toInt
//    println(result)
    result
  }

  def part1(input: List[Array[Char]]): Int = {
    input.map(findMax2).sum
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = input.map(_.toCharArray)
    println(parsed)
    println(part1(parsed))
