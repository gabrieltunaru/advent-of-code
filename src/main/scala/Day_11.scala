package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_11:
  case class Operation(left: String, op: String, right: String)
  case class MonkeyInput(number: Int, items: List[Int], operation: Operation, test: Int, trueMonkey: Int, falseMonkey: Int)
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
      val divided = (increasedStressLevel.toFloat / 3).round
      if (divided % monkeyInput.test == 0) MonkeyMove(increasedStressLevel,monkeyInput.number, monkeyInput.trueMonkey)
      else MonkeyMove(increasedStressLevel, monkeyInput.number, monkeyInput.trueMonkey)
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
    monkeys.foldLeft(monkeys)((currentMonkeys, monkeyInput)=>
      val moves = parseMonkeyItem(monkeyInput)
      throwItem(currentMonkeys, moves)
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

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val grouped = input.grouped(7)
    val parsed = grouped.map(parseMonkey).toList
    val result = inspectItems(parsed)
    println(result)
