package com.cannondev.advent

import org.scalatest.flatspec.AnyFlatSpec
import Day_11.*

class Day11Test extends AnyFlatSpec:


  it should "correctly parse the operator" in {
    assert(Day_11.getOperator(1, "old") == 1)
    assert(Day_11.getOperator(1, "3") == 3)
  }

  it should "correctly apply operation" in {
    val product1 = Operation("2", "*", "old")
    val product2 = Operation("old", "*", "old")
    val product3 = Operation("old", "*", "5")
    val sum1 = Operation("2", "+", "old")
    val sum2 = Operation("old", "+", "old")
    val sum3 = Operation("old", "+", "5")
    assert(applyOperation(3, product1) == 6)
    assert(applyOperation(3, product2) == 9)
    assert(applyOperation(3, product3) == 15)
    assert(applyOperation(3, sum1) == 5)
    assert(applyOperation(3, sum2) == 6)
    assert(applyOperation(3, sum3) == 8)
  }

  it should "correctly generate the move" in {
    val input = MonkeyInput(0,List(79, 98),Operation("old","*","19"),23,2,3)
    val expectedMoves = List(MonkeyMove(500,0,3), MonkeyMove(620,0,3))
    val actual = parseMonkeyItem(input)
    assert(actual==expectedMoves)
  }


