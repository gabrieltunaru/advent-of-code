package com.cannondev.advent

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day2_2 {
  val resources = "src/main/resources"
  val index = 2
  val filePath = s"$resources/day_$index.txt"

  val aScore = Map("X" -> 3, "Y" -> 6, "Z" -> 0)
  val bScore = Map("X" -> 0, "Y" -> 3, "Z" -> 6)
  val cScore = Map("X" -> 6, "Y" -> 0, "Z" -> 3)

  val chooseMap = Map("X" -> 1, "Y" -> 2, "Z" -> 3)

  val outcomeMap = Map("A" -> aScore, "B" -> bScore, "C" -> cScore)


  val aChoice = Map("X" -> "Z", "Y" -> "X", "Z" -> "Y")
  val bChoice = Map("X" -> "X", "Y" -> "Y", "Z" -> "Z")
  val cChoice = Map("X" -> "Y", "Y" -> "Z", "Z" -> "X")

  val choiceMap = Map("A" -> aChoice, "B" -> bChoice, "C" -> cChoice)

  def parse(s: String): (String, String) =
    s.split(" ") match
      case Array(l, r) => (l, r)
      case _ => throw new Error("invalid input")

  def calculateScore(l: List[(String, String)]): Int = l.foldLeft(0)((acc, el) => el match
    case (left, yourChoice) =>
      val outcomeScore = outcomeMap(left)(yourChoice)
      val chooseScore = chooseMap(yourChoice)
        outcomeScore +chooseScore + acc
  )


  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val parsed = fileContents.map(parse)
    val calculatedMoves = parsed.map{case (his, yours) => (his, choiceMap(his)(yours))}
    val score = calculateScore(calculatedMoves)
    println(score)

}
