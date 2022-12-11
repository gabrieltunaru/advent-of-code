package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_11:
  case class Operation(left: String, op: String, right: String)
  case class MonkeyInput(
      number: Int,
      items: List[Int],
      operation: Operation,
      test: Int,
      trueMonkey: Int,
      falseMonkey: Int,
      inspectedNo: Int = 0
  )
  case class MonkeyMove(stressLevel: Int, sourceMonkey: Int, targetMonkey: Int)

  val index = 11

  def getOperator(old: Int, operator: String): Int =
    operator match
      case "old" => old
      case s     => s.toInt

  def applyOperation(stressLevel: Int, operation: Operation): Int =
    operation.op match
      case "*"     => getOperator(stressLevel, operation.left) * getOperator(stressLevel, operation.right)
      case "+"     => getOperator(stressLevel, operation.left) + getOperator(stressLevel, operation.right)
      case another => throw new Error(s"Invalid operator: $another in $operation")

  def parseMonkeyItem(monkeyInput: MonkeyInput): List[MonkeyMove] =
    monkeyInput.items.map(stressLevel =>
      val increasedStressLevel = applyOperation(stressLevel, monkeyInput.operation)
      val divided = (increasedStressLevel.toFloat / 3).floor.round
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

  def inspectItems(monkeys: List[MonkeyInput]): List[MonkeyInput] =
    monkeys
      .map(_.number)
      .foldLeft(monkeys)((currentMonkeys, number) =>
        val monkeyInput = currentMonkeys(number)
        val moves = parseMonkeyItem(monkeyInput)
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
    (1 to rounds).foldLeft(List(monkeyInputs))((acc, _) => inspectItems(acc.head) :: acc)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val grouped = input.grouped(7)
    val parsed = grouped.map(parseMonkey).toList
    println(parsed)
//    val result = inspectItems(parsed)

    val after20Rounds = throwItemsForXRounds(parsed, 20)
    after20Rounds.reverse.zipWithIndex.foreach((l, i) =>
      println(i)
      l.map(_.items).foreach(println)
      println("")
    )
    val res = after20Rounds.flatMap(_.map(monkey => (monkey.number, monkey.items)))
    println(res)
    val summed = res.foldLeft(Map.empty[Int, List[Int]])((acc, el) =>
      val (number, items) = el
      val before = acc.getOrElse(number, List.empty[Int])
      acc.updated(number, before ++ items)
    )
    val counted = summed.map((k, v) => (k, v.size))
    println(summed)
    println(counted)
    val actualRes = after20Rounds.head.map(m => (m.number, m.inspectedNo))
    println(actualRes)
    val biggest2 = actualRes.map(_._2).sorted.reverse.slice(0,2).product
    println(biggest2)