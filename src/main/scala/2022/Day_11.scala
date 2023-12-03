package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_11:
  case class Operation(left: String, op: String, right: String)
  case class MonkeyInput(
      number: Int,
      items: List[BigInt],
      operation: Operation,
      test: Int,
      trueMonkey: Int,
      falseMonkey: Int,
      inspectedNo: Int = 0
  )
  case class MonkeyMove(stressLevel: BigInt, sourceMonkey: Int, targetMonkey: Int)

  val index = 11

  def getOperator(old: BigInt, operator: String): BigInt =
    operator match
      case "old" => old
      case s     => s.toInt

  def applyOperation(stressLevel: BigInt, operation: Operation): BigInt =
    operation.op match
      case "*"     => getOperator(stressLevel, operation.left) * getOperator(stressLevel, operation.right)
      case "+"     => getOperator(stressLevel, operation.left) + getOperator(stressLevel, operation.right)
      case another => throw new Error(s"Invalid operator: $another in $operation")

  def parseMonkeyItem(monkeyInput: MonkeyInput, reducer: Int): List[MonkeyMove] =
    monkeyInput.items.map(stressLevel =>
      val increasedStressLevel = applyOperation(stressLevel, monkeyInput.operation)
      val divided = increasedStressLevel % reducer
      if (divided % monkeyInput.test == 0) MonkeyMove(divided, monkeyInput.number, monkeyInput.trueMonkey)
      else MonkeyMove(divided, monkeyInput.number, monkeyInput.falseMonkey)
    )

  def parseMonkey(lines: List[String]): MonkeyInput =

    val monkey = "Monkey (.*):".r
    val items = " *Starting items: (.*)".r
    val operation = " *Operation: new = (.*) (.) (.*)".r
    val test = " *Test: divisible by (.*)".r
    val trueCase = " *If true: throw to monkey (.*)".r
    val falseCase = " *If false: throw to monkey (.*)".r

    val monkeyInput = lines.slice(0, 6)

    monkeyInput match
      case List(
            monkey(number),
            items(stringItems),
            operation(left, op, right),
            test(divisibleBy),
            trueCase(trueValue),
            falseCase(falseValue)
          ) =>
        MonkeyInput(
          number = number.toInt,
          items = stringItems.split(",").map(_.strip()).toList.map(_.toInt),
          operation = Operation(left, op, right),
          test = divisibleBy.toInt,
          trueMonkey = trueValue.toInt,
          falseMonkey = falseValue.toInt
        )
      case e => throw new Error(s"invalid input: $e")

  def inspectItems(monkeys: List[MonkeyInput], reducer: Int): List[MonkeyInput] =
    monkeys
      .map(_.number)
      .foldLeft(monkeys)((currentMonkeys, number) =>
        val monkeyInput = currentMonkeys(number)
        val moves = parseMonkeyItem(monkeyInput, reducer)
        val afterThrowing = throwItem(currentMonkeys, moves)
        val newMonkeyInput = afterThrowing(monkeyInput.number)
        val updated = newMonkeyInput.copy(inspectedNo = monkeyInput.inspectedNo + monkeyInput.items.size)
        afterThrowing.updated(monkeyInput.number, updated)
      )

  def throwItem(monkeys: List[MonkeyInput], moves: List[MonkeyMove]): List[MonkeyInput] = {
    moves.foldLeft(monkeys)((currentMonkeys, move) =>
      val sourceMonkey = currentMonkeys(move.sourceMonkey)
      val targetMonkey = currentMonkeys(move.targetMonkey)
      val updatedMonkey = targetMonkey.copy(items = targetMonkey.items.appended(move.stressLevel))
      val nextMonkeys = currentMonkeys.updated(move.sourceMonkey, sourceMonkey.copy(items = List.empty))
      nextMonkeys.updated(move.targetMonkey, updatedMonkey)
    )
  }

  def throwItemsForXRounds(monkeyInputs: List[MonkeyInput], rounds: Int) =
    val reducer = monkeyInputs.map(_.test).product
    (1 to rounds).foldLeft(List(monkeyInputs))((acc, _) => inspectItems(acc.head, reducer) :: acc)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val grouped = input.grouped(7)
    val parsed = grouped.map(parseMonkey).toList
    println(parsed)

    val afterRounds = throwItemsForXRounds(parsed, 10000)

    afterRounds.reverse.zipWithIndex.foreach((l, i) =>
      if(i%1000==0)
        println(i)
        l.map(_.items).foreach(println)
        println("")
    )

    val actualRes = afterRounds.head.map(m => (m.number, m.inspectedNo))
    println(actualRes)
    val biggest2 = actualRes.map(_._2).sorted.reverse.slice(0,2).map(BigInt(_)).product
    println(biggest2)