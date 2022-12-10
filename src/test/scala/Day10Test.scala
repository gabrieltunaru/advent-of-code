package com.cannondev.advent

import org.scalatest.flatspec.AnyFlatSpec

class Day10Test extends AnyFlatSpec:

  val input: String =
    """
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop
      |""".stripMargin

  val smallInput: List[String] =
    """
      |noop
      |addx 1
      |addx 1
      |noop
      |""".stripMargin.split("\\n").tail.toList

  "Signal intervals" should "be correctly generated" in {
    val intervals240 = Day10.getSignalIntervals(240)
    val expected240 = List(20, 60, 100, 140, 180, 220)
    val intervals21 = Day10.getSignalIntervals(21)
    val expected21 = List(20)
    assert(intervals240==expected240)
    assert(intervals21==expected21)
  }

