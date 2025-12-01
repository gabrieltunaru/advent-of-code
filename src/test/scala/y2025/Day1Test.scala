package com.cannondev.advent
package y2025

import com.cannondev.advent.util.FileReader
import com.cannondev.advent.y2025.Day_1.{Direction, Rotation}
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {
  test("should rotate correctly") {
    assert(Day_1.rotate(11, List(Rotation(Direction.Right, 8))).current == 19)
    assert(
      Day_1
        .rotate(
          11,
          List(
            Rotation(Direction.Right, 8),
            Rotation(Direction.Left, 19)
          )
        )
        .current == 0
    )
    assert(
      Day_1
        .rotate(
          11,
          List(
            Rotation(Direction.Right, 8),
            Rotation(Direction.Left, 19),
            Rotation(Direction.Left, 1)
          )
        )
        .current == 99
    )
    assert(
      Day_1
        .rotate(
          5,
          List(
            Rotation(Direction.Left, 10),
            Rotation(Direction.Right, 5)
          )
        )
        .current == 0
    )

    assert(
      Day_1
        .rotate(
          50,
          List(
            Rotation(Direction.Right, 49),
            Rotation(Direction.Right, 1),
            Rotation(Direction.Left, 99),
            Rotation(Direction.Left, 1)
          )
        )
        == Day_1.Counter(0, 2)
    )
    assert(
      Day_1
        .rotate(
          0,
          List(
            Rotation(Direction.Right, 967)
          )
        )
        == Day_1.Counter(67, 0)
    )
    assert(
      Day_1
        .rotate(
          50,
          List(
            Rotation(Direction.Right, 49),
            Rotation(Direction.Right, 1),
            Rotation(Direction.Right, 100),
            Rotation(Direction.Right, 200),
            Rotation(Direction.Right, 250)
          )
        )
        == Day_1.Counter(50, 3)
    )
//    assert(
//      Day_1
//        .rotate(
//          50,
//          List(
//            Rotation(Direction.Left, 49),
//            Rotation(Direction.Left, 1),
//            Rotation(Direction.Left, 100),
//            Rotation(Direction.Left, 200),
//            Rotation(Direction.Left, 250)
//          )
//        )
//        == Day_1.Counter(50, 3)
//    )
    assert(
      Day_1
        .rotate(
          50,
          List(
            Rotation(Direction.Left, 50),
            Rotation(Direction.Left, 100),
          )
        )
        == Day_1.Counter(0, 2)
    )
  }

  test("should not account for intermediary zeros") {
    assert(Day_1.rotate(5, List(Rotation(Direction.Left, 101))).numberOfZeros == 0)
    assert(Day_1.rotate(5, List(Rotation(Direction.Left, 105))).numberOfZeros == 1)
    assert(Day_1.rotate(5, List(Rotation(Direction.Left, 5), Rotation(Direction.Right, 100))).numberOfZeros == 1)
  }

  test("Should work on example") {
    val input = FileReader.readLines(1, 2025)
    val parsed = Day_1.parse(input)
    val result = Day_1.rotate(50, parsed)
    assert(result.numberOfZeros == 3)
    assert(result.current == 32)
  }

}
