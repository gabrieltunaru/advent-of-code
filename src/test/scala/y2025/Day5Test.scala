package com.cannondev.advent
package y2025

import util.FileReader

import cats.Semigroup
import cats.syntax.all.*
import com.cannondev.advent.Day_5.*
import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {
  test("it should work") {
    val x = Interval(1, 2)
    val y = Interval(3, 4)
    assert(union(x,y) == 0)
    assert(union(y,x) == 0)
  }

  test("should find underlying") {
    val x = Interval(1, 2)
    val y = Interval(2, 4)
    assert(union(x,y) == 1)
    assert(union(y,x) == 1)
  }


  test("should find underlying") {
    val x = Interval(1, 2)
    val y = Interval(2, 4)
    val z = Interval(2, 4)
    val t = Interval(2, 4)
    
    assert(union(x,y) == 1)
    assert(union(y,x) == 1)
  }
}
