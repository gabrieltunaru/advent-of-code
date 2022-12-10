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
    val intervals240 = Day10_1.getSignalIntervals(240)
    val expected240 = List(20, 60, 100, 140, 180, 220)
    val intervals21 = Day10_1.getSignalIntervals(21)
    val expected21 = List(20)
    assert(intervals240==expected240)
    assert(intervals21==expected21)
  }

  "Power" should "correctly be calculated" in {
    val lines = input.split("\\n").tail.toList // skip first empty line
    val initialStates = List(State(179, 16, None))
    val res = Day10_1.move(lines, initialStates)

    val smallRes = Day10_1.move(smallInput, List(State(0,1, None))).reverse
    assert(smallRes.contains(State(1,1,Some("noop"))))
    assert(smallRes.contains(State(2, 1, Some("addx 1"))))
    assert(smallRes.contains(State(3, 2, Some("addx 1"))))
    assert(smallRes.contains(State(4, 2, Some("addx 1"))))
    assert(smallRes.contains(State(5, 3, Some("addx 1"))))
    assert(smallRes.contains(State(6, 3, Some("noop"))))

  }
