package com.cannondev.advent
package y2025

import util.FileReader
import y2025.Day_1.{Direction, Rotation}
import y2025.Day_2.*

import com.cannondev.advent.y2025.Day_3.findMax
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {
  test("it should work") {
    val input = List("123".toCharArray)
    val actual = input.map(x => findMax(x.toList, 3)).sum
    val expected = 123
    assert(actual==expected)
  }

}
