package com.cannondev.advent

import scala.collection.immutable.Set
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day3_1 {
  val resources = "src/main/resources"
  val index = 3
  val filePath = s"$resources/day_$index.txt"


  def parse(s: String): Char =
    val (l,r) = s.splitAt(s.length()/2)
    l.toSet.intersect(r.toSet).toList.head

  def getScore(c: Char): Int =
    if (c.isLower) c-'a'+1
    else c-'A'+27

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val parsed = fileContents.map(parse)
    def scores = parsed.map(getScore)
    println(scores.sum)

}
