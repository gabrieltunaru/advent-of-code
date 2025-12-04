package com.cannondev.advent
package y2025

import util.FileReader
import com.cannondev.advent.y2025.Day_4.Position
import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite
import cats.syntax.all.*
import Day_4.Position.semigroup
import cats.Semigroup

class Day4Test extends AnyFunSuite {
  test("it should work") {
    val x = Position(1, 2)
    val y = Position(1, 2)
    assert((x |+| y) == Position(2, 4))
  }

}
