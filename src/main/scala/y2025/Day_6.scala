package com.cannondev.advent
package y2025

import util.FileReader

import scala.annotation.tailrec
import scala.io.Source

object Day_6:
  case class Input(numbers: List[List[BigInt]], ops: List[String])

  val index = 6

  def mapNumbers(s: String) = s.split(" +").filter(_.nonEmpty).map(ss => BigInt(ss)).toList

  def parse(lines: List[String]): Input =
    val numberLines = lines.slice(0, lines.length - 1)
    val opLine = lines.last

    Input(
      numberLines.map(mapNumbers).toList,
      opLine.split(" +").filter(_.nonEmpty).toList
    )

  @tailrec
  def part1(numbers: List[List[BigInt]], ops: List[String], acc: BigInt): BigInt = {
    ops.headOption match {
      case Some("*") => part1(numbers.tail, ops.tail, acc + numbers.head.product)
      case Some("+") => part1(numbers.tail, ops.tail, acc + numbers.head.sum)
      case None      => acc
      case Some(s)   => throw new Error(s"Invalid operator $s")
    }
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed.numbers.transpose, parsed.ops, 0))
