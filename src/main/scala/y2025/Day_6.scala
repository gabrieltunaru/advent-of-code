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

  def constructArray(
      input: List[List[Char]],
      bigAcc: List[List[BigInt]],
      smallAcc: List[BigInt]
  ): List[List[BigInt]] = {
    input match {
      case head :: tail =>
        if (!head.exists(_.isDigit)) constructArray(tail, smallAcc :: bigAcc, Nil)
        else constructArray(tail, bigAcc, BigInt(head.filter(_.isDigit).mkString) :: smallAcc)
      case Nil => if (smallAcc.isEmpty) bigAcc else smallAcc :: bigAcc
    }
  }

  def part2(lines: List[String]): BigInt =
    val numberLines = lines.slice(0, lines.length - 1)
    val opLine = lines.last

    val transposed = numberLines.map(_.toCharArray).transpose
//    transposed.foreach(println)
//    println(constructArray(transposed, Nil, Nil))
//    println(transposed.map(s => BigInt(s.filter(_.isDigit).mkString)))

    val numbers = constructArray(transposed, Nil, Nil).reverse
    val ops = opLine.split(" +").filter(_.nonEmpty).toList
    val result = part1(numbers, ops, 0)
//    println(numbers)
//    println(result)
    result

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed.numbers.transpose, parsed.ops, 0))
    println(part2(input))
