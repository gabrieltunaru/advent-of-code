package com.cannondev.advent

import util.FileReader

import scala.io.Source


object DayTemplate:
  case class Something(value: Int)

  val index = 1

  def parse(lines: List[String]): List[Something] =

    val pattern = "U (.*)".r

    val line = lines.head

    line match
      case pattern(v) => List(Something(v.toInt))
      case e          => throw new Error(s"invalid input: $e")

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val parsed = parse(input)
    println(parsed)
