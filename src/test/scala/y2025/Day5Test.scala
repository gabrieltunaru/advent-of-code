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
    assert(union(x, y) == 0)
    assert(union(y, x) == 0)
  }

  test("should find underlying") {
    val x = Interval(1, 2)
    val y = Interval(2, 4)
    assert(union(x, y) == 1)
    assert(union(y, x) == 1)
  }

  test("should find underlying?") {
    val x = Interval(1, 2)
    val y = Interval(2, 4)
    val z = Interval(2, 4)
    val t = Interval(2, 4)

    assert(union(x, y) == 1)
    assert(union(y, x) == 1)
  }

  test("should merge interval") {
    val a = Interval(1, 3)
    val b = Interval(3, 4)
    assert(mergeIntervals(a, b) == Interval(1, 4))
  }

  test("should not find interval") {
    val a = Interval(10, 14)
    val b = Interval(16, 20)
    assert(find(List(a, b)) == List(a, b))
  }

  test("should find interval") {
    val t = Interval(1, 2)
    val a = Interval(3, 4)
    val b = Interval(4, 20)
    val c = Interval(19, 21)
    val r = Interval(22, 29)
    val d = Interval(30, 31)
    assert(
      find(List(t, a, b, c, r, d)).toSet == Set(Interval(3, 21), Interval(22, 29), Interval(1, 2), Interval(30,31))
    )
  }
}
