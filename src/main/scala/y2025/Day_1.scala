package com.cannondev.advent
package y2025

import util.FileReader

import com.cannondev.advent.y2025.Day_1.Direction

import scala.io.Source

object Day_1:
  enum Direction {
    case Left
    case Right
  }
  case class Rotation(dir: Direction, value: Int)

  val index = 1

  def rotate(start: Int, rotations: List[Rotation]): Int = {
    rotations.foldLeft(start) { case (acc, rotation) =>
      val rotated = rotation.dir match
        case Direction.Left  => acc - rotation.value
        case Direction.Right => acc + rotation.value
      val adjusted =
        if (rotated > 99) rotated - 100
        else if (rotated < 0) rotated + 100
        else rotated
      adjusted
    }
  }

  def parse(lines: List[String]): List[Rotation] =
    lines.map(line => {
      val dir = line.head match
        case 'L' => Direction.Left
        case 'R' => Direction.Right
        case c   => throw new Error(s"unable to parse direction $c")
      val value = line.tail.toInt
      Rotation(dir, value)
    })

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(rotate(50, parsed))
