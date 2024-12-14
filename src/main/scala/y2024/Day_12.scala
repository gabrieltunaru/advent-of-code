package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_12:
  case class Something(x: Int, y: Int, value: Char)

  val index = 12

  def parse(lines: List[String]): List[Something] =
    lines.zipWithIndex.flatMap((s, i) => s.toCharArray.toList.zipWithIndex.map((c, j) => Something(i, j, c)))

//  def part1()

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val parsed = parse(input)
    println(parsed)
