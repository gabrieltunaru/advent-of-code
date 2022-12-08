package com.cannondev.advent

import scala.collection.immutable.Set
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day4_1 {
  val resources = "src/main/resources"
  val index = 4
  val filePath = s"$resources/day_$index.txt"

  def getRange(s: String): List[Int] =
    s.split("-") match
      case Array(l, r) => List.range(l.toInt, r.toInt+1)
      case _           => throw new Error("invalid input")

  def parse(s: String): (List[Int], List[Int]) =
    s.split(",") match
      case Array(l, r) => (getRange(l), getRange(r))
      case _           => throw new Error("invalid input")

  def areSubsets(first: List[Int], second: List[Int]): Boolean =

    val r1 = first.toSet
    val r2 = second.toSet

    r1.subsetOf(r2) || r2.subsetOf(r1)

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val parsed: List[(List[Int], List[Int])] = fileContents.map(parse)
    val subsets = parsed.map(areSubsets)
    val subsetsNo = subsets.map(if (_) 1 else 0).sum
    println(subsetsNo)

}
