package com.cannondev.advent

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day2 {
  val resources = "src/main/resources"
  val index = 2
  val filePath = s"$resources/day_$index.txt"

  val aMap = Map("X" -> 3, "Y" -> 6, "Z" -> 0)
  val bMap = Map("X" -> 0, "Y" -> 3, "Z" -> 6)
  val cMap = Map("X" -> 6, "Y" -> 0, "Z" -> 3)

  val chooseMap = Map("X" -> 1, "Y" -> 2, "Z" -> 3)

  val outcomeMap = Map("A" -> aMap, "B" -> bMap, "C" -> cMap)

  def parse(s: String): (String, String) =
    s.split(" ") match
      case Array(l,r) => (l,r)
      case _ => throw new Error("invalid input")

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val sumPerElf = fileContents.foldLeft(0)((acc, el) =>
      val (left, yourChoice) = parse(el)
      val outcomeScore = outcomeMap(left)(yourChoice)
      val chooseScore = chooseMap(yourChoice)

      outcomeScore + chooseScore + acc
    )
    println(sumPerElf)

}
