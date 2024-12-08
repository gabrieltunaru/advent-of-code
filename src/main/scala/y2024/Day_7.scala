package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_7:
  case class Something(goal: BigInt, numbers: List[BigInt])
  enum Operations {
    case Add
    case Multiply
  }

  def getOperations(n: Int): Set[List[Operations]] =
    val operations = for {
      i <- 0 to n
    } yield List.fill(i)(Operations.Add) ++ List.fill(n - i)(Operations.Multiply)
//    val reverseOperatios = operations.map(_.reverse)
    operations.flatMap(_.permutations).toSet

  val index = 7

  def parse(lines: List[String]): List[Something] =
    lines.map(l => {
      val splitted = l.split(": ")
      val goal = BigInt(splitted(0))
      val numbers = splitted(1).split(" ").map(BigInt(_))
      Something(goal, numbers.toList)
    })

  def applyOperation(input: Something, operations: List[Operations]): BigInt =
    val appliedOperation =
      input.numbers.tail.zip(operations).foldLeft(input.numbers.head) { case (acc, (number, operation)) =>
        operation match
          case Operations.Add      => acc + number
          case Operations.Multiply => acc * number
      }
    appliedOperation

  def part1(input: List[Something]): List[BigInt] =
    for {
      s <- input
      operations = getOperations(s.numbers.length)
      isValid = operations.exists(o => applyOperation(s, o) == s.goal)
    } yield if (isValid) s.goal else 0

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
//    println(parsed)
    println(getOperations(3))
    println(applyOperation(Something(7, List(3, 4)), List(Operations.Multiply)))
    println(List(Operations.Add, Operations.Add, Operations.Multiply).permutations.toList)
    println(part1(parsed).sum)
