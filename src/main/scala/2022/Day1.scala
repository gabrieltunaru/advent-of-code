package com.cannondev.advent

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day1 {
  val resources = "src/main/resources"
  val filename = "day_1.txt"
  val filePath = s"$resources/$filename"

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val sumPerElf = fileContents.foldRight(List.empty[Int])((stringValue, tail) =>
      Try(stringValue.toInt) match
        case Failure(_)     => 0 :: tail
        case Success(value) => tail.dropRight(1).headOption.getOrElse(0) + value :: tail
    )
    val sorted = sumPerElf.sorted.takeRight(3)
    
    println(sumPerElf.max)
    println(sorted.sum)

}
