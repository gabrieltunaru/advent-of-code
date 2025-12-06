package com.cannondev.advent
package y2025

import util.FileReader

import cats.Semigroup
import cats.syntax.all.*
import com.cannondev.advent.Day_5.*
import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {



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
      find(List(t, a, b, c, r, d)).toSet == Set(Interval(3, 21), Interval(22, 29), Interval(1, 2), Interval(30, 31))
    )
  }

  test("why doesn't it work") {
    val a = Interval(1, 3)
    val b = Interval(4, 5)
    val c = Interval(5, 6)
    val d = Interval(5, 7)
    val e = Interval(4, 8)
    val f = Interval(5, 8)
    val input = List(a, b, c, d, e, f)
    assert(find(input).toSet == Set(Interval(1, 3), Interval(4, 8)))
  }

  test("it should merge") {
    val input = List(Interval(4, 5), Interval(3, 4), Interval(1, 2), Interval(2, 3))
    assert(find(input) == List(Interval(1, 5)))
  }

  test("it shouldn't merge") {
    val input = List(Interval(1, 2), Interval(3, 4), Interval(4, 5), Interval(5, 6), Interval(7, 8))
    assert(find(input).toSet == Set(Interval(1, 2), Interval(3, 6), Interval(7, 8)))
  }

  test("drawings") {
    assert(find(List(Interval(1, 5), Interval(2, 6))) == List(Interval(1, 6)))
    assert(find(List(Interval(1, 5), Interval(6, 8))) == List(Interval(1, 5), Interval(6, 8)))
    assert(find(List(Interval(1, 5), Interval(5, 7))) == List(Interval(1, 7)))
    assert(find(List(Interval(1, 5), Interval(2, 2))) == List(Interval(1, 5)))
    assert(find(List(Interval(1, 5), Interval(0, 1))) == List(Interval(0, 5)))
    assert(find(List(Interval(0, 1), Interval(1, 5))) == List(Interval(0, 5)))
    assert(find(List(Interval(2, 4), Interval(2, 4))) == List(Interval(2, 4)))
    assert(
      find(List(Interval(2, 4), Interval(2, 4))) == List(Interval(2, 4))
    )
    assert(
      find(
        List(
          Interval(2, 4),
          Interval(2, 4),
          Interval(4, 5),
          Interval(4, 6),
          Interval(6, 7),
          Interval(3, 4),
          Interval(3, 4),
          Interval(1, 2),
          Interval(1, 7),
          Interval(2, 3),
          Interval(1, 7),
          Interval(1, 1),
          Interval(1, 2),
          Interval(7, 7)
        )
      ) ==
        List(Interval(1, 7))
    )

  }
}
