package com.cannondev.advent
package y2025

import util.FileReader
import y2025.Day_1.{Direction, Rotation}

import com.cannondev.advent.y2025.Day_2.Interval
import org.scalatest.funsuite.AnyFunSuite
import Day_2.*

class Day2Test extends AnyFunSuite {
 test("should find duplicated in 11-22") {
   val expanded = expandInterval(Interval(11,22))
   val duplicates = expanded.filter(x => isRepeating(x))
   assert(duplicates== List(11,22))
 }

  test("should find duplicated in 95-115") {
    val expanded = expandInterval(Interval(95, 115))
    val duplicates = expanded.filter(x => isRepeating(x))
    assert(duplicates == List(99))
  }

  test("should find duplicated in 998-1012") {
    val expanded = expandInterval(Interval(998,1012))
    val duplicates = expanded.filter(x => isRepeating(x))
    assert(duplicates == List(1010))
  }

}
