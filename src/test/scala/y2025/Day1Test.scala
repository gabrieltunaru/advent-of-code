package com.cannondev.advent
package y2025

import com.cannondev.advent.y2025.Day_1.{Direction, Rotation}
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {
  test("should rotate correctly") {
    assert(Day_1.rotate(11, List(Rotation(Direction.Right, 8))).current == 19)
    assert(Day_1.rotate(11, List(
      Rotation(Direction.Right, 8),
      Rotation(Direction.Left, 19),
    )
    ).current == 0)
    assert(Day_1.rotate(11, List(
      Rotation(Direction.Right, 8),
      Rotation(Direction.Left, 19),
      Rotation(Direction.Left, 1),
    )
    ).current == 99)
    assert(Day_1.rotate(5, List(
      Rotation(Direction.Left, 10),
      Rotation(Direction.Right, 5),
    )
    ).current == 0)
  }
}
