package com.cannondev.advent

import scala.collection.immutable.Set
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day3_2 {
  val resources = "src/main/resources"
  val index = 3
  val filePath = s"$resources/day_$index.txt"

  def findIntersection(l: List[String]): Char =
    val s = l.map(_.toSet)
    s.foldLeft(s.head)((a, b) => a.intersect(b)).head

  def getScore(c: Char): Int =
    if (c.isLower) c - 'a' + 1
    else c - 'A' + 27

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val grouped = fileContents.grouped(3).map(findIntersection).toList
    val scores = grouped.map(getScore)

    println(scores.sum)

}
