package com.cannondev.advent
package y2025

import util.FileReader

object Day_1:
  enum Direction {
    case Left
    case Right
  }
  case class Rotation(dir: Direction, value: Int)
  case class Counter(current: Int, numberOfZeros: BigInt)

  val index = 1

  def rotate(start: Int, rotations: List[Rotation]): Counter = {
    rotations.foldLeft(Counter(start, 0)) { case (acc, rotation) =>
      val rotated = rotation.dir match
        case Direction.Left  => acc.current - rotation.value
        case Direction.Right => acc.current + rotation.value
      val adjusted =
        if (rotated > 99) rotated % 100
        else if (rotated < 0) (100 + rotated % 100) % 100
        else rotated
      val numberOfZeros = adjusted match
        case 0 => acc.numberOfZeros + 1
        case _ => acc.numberOfZeros
      Counter(adjusted, numberOfZeros)
    }
  }

  def rotateP2(start: Int, rotations: List[Rotation]): Counter = {
    rotations.foldLeft(Counter(start, 0)) { case (acc, rotation) =>
      val intermediaryZeroes = rotation.value / 100
      val adjustedRotation = rotation.value  % 100
      val rotated = rotation.dir match
        case Direction.Left  => acc.current -adjustedRotation
        case Direction.Right => acc.current +adjustedRotation
      val adjusted =
        if (rotated > 99) rotated % 100
        else if (rotated < 0) (100 + rotated % 100) % 100
        else rotated

      val changedSign = if (adjusted != rotated && acc.current != 0 && rotated % 100 != 0) 1 else 0
      val numberOfZeros = adjusted match
        case 0 if adjustedRotation == 0 => acc.numberOfZeros + intermediaryZeroes + changedSign
        case 0                              => acc.numberOfZeros + 1 + intermediaryZeroes + changedSign
        case _                              => acc.numberOfZeros + intermediaryZeroes + changedSign
      Counter(adjusted, numberOfZeros)
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

  // Used: 893
  // P2: 6163, 5531, 5978
  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(rotate(50, parsed))
    println(rotateP2(50, parsed))
