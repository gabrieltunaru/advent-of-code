package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_8:
  case class Input(instructions: LazyList[Char], map: Map[String, Directions])
  case class Directions(left: String, right: String)

  val index = 8

  def parse(lines: List[String]): Input =

    val pattern = "(.*) = \\((.*), (.*)\\)".r

    val instructions = lines.head.toCharArray
    val infiniteInstructions = LazyList.continually(instructions).flatten

    val pairs = lines.tail.tail.map {
      case pattern(k, l, r) => k -> Directions(l, r)
      case e                => throw new Error(s"invalid input: $e")
    }
    Input(infiniteInstructions, pairs.toMap)

  def part1(
      current: String,
      end: String,
      steps: Int,
      instructions: LazyList[Char],
      map: Map[String, Directions]
  ): Int =
    val instruction = instructions.head
    if (current == end) steps
    else
      val nextStep = instruction match
        case 'L' => map(current).left
        case 'R' => map(current).right
      part1(nextStep, end, steps + 1, instructions.tail, map)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    println(part1("AAA", "ZZZ", 0, parsed.instructions, parsed.map))
