package com.cannondev.advent
package util

import scala.io.Source

object FileReader {

  val resources = "src/main/resources"

  def readString(index: Int, year: Int = 2022): String =
    val filePath = s"$resources/day_$index.txt"
    val source = Source.fromFile(filePath)
    val read = source.mkString
    source.close()
    read

  def readLines(index: Int, year: Int=2022): List[String] =
    val filePath = s"$resources/$year/day_$index.txt"
    val source = Source.fromFile(filePath)
    val read = source.getLines().toList
    source.close()
    read
}
