package com.cannondev.advent

import util.FileReader

import scala.io.Source


object Day_1:
  case class Something(value: Int)

  val index = 1

  val digitMap = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  def parse(lines: List[String]): List[Something] =
    val filtered = lines.map(_.filter(_.isDigit))
    val numbers = filtered.map(line => {
      val first = line.charAt(0)
      val last = line.charAt(line.length-1)
      s"$first$last".toInt
    })
    println(numbers.sum)
    Nil

  def replace(line: String, map: Map[String, Int]): String =
    line.foldLeft("")((stringAcc, char) => {
      val temp = stringAcc + char
      map.foldLeft(temp) { case (acc, (word, int)) =>
        acc.replace(word, int.toString + word)
      }
    })


  def part2(lines: List[String]): Int=
    val replaced = lines.map(line => {
     replace(line, digitMap)
    })
    val filtered = replaced.map(_.filter(_.isDigit))

    println(replaced)
    val numbers = filtered.map(line => {
      val first = line.charAt(0)
      val last = line.charAt(line.length - 1)
      s"$first$last".toInt
    })
    println(numbers)

    numbers.sum



  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
//    val parsed = parse(input)
//    println(parsed)
    println(part2(input))
