package com.cannondev.advent
package y2025
import util.FileReader

import scala.io.Source

object Day_3:
  case class Something(value: Int)

  val index = 3

  def findMax2(c: Array[Char]): Int = {
    val withIndex = c.zipWithIndex
    val max = withIndex.reverse.tail.reverse.maxBy(_._1)
    val secondMax = withIndex.filterNot((_, i) => i <= max._2).maxBy(_._1)
    val result = List(max, secondMax).sortBy(_._2).map(_._1).mkString.toInt
    result
  }

//  def findMax(c: List[Char], digits: Int): BigInt = {
//    val withIndex = c.zipWithIndex
//    //
//    val max = withIndex.filter((_, i) => i <= c.length - digits).maxBy(_._1)
//    val secondMax = (0 until digits - 1).foldLeft(List.empty[(Char, Int)])((acc, el) => {
//      val charsAfterMax = withIndex.filter((_, i) => i > max._2 + el && !acc.map(_._2).contains(i))
//      val nextMax = charsAfterMax.maxBy(_._1)
//      nextMax :: acc
//    })
//    val result = BigInt((max :: secondMax).sortBy(_._2).map(_._1).mkString)
//    result
//  }
//
  // ne trebuie maximul din intervalul dintre start (indexul nr precedent sau 0, cat sa aiba destule dupa ele pentru a forma restul cifrelor)
  // si stop (ultimul index dupa care mai sunt nr cat sa formeze toate cifrele)
  def findMax(c: List[Char], digits: Int): BigInt = {
    val withIndex = c.zipWithIndex
    //
    val secondMax = (0 until digits ).foldLeft(List.empty[(Char, Int)])((acc, el) => {
      val start = acc.map(_._2).headOption.getOrElse(-1)
      val remainingDigits = digits - acc.length
      val stop = c.length - remainingDigits
      val filtered = withIndex.filter((_, i) => i > start && i <= stop)
      val nextMax = filtered.maxBy(_._1)
      nextMax :: acc
    })
    val result = BigInt(secondMax.sortBy(_._2).map(_._1).mkString)
    result
  }

  def part1(input: List[Array[Char]]): Int = {
    input.map(findMax2).sum
  }

  // 172770750974268,
  def part2(input: List[Array[Char]]): BigInt = {
    input.map(x => findMax(x.toList, 12)).sum
  }

  def test(input: List[Array[Char]]) =
    val p1 = input.map(findMax2)
    val p2 = input.map(i => findMax(i.toList, 2))
    p1.zip(p2).zipWithIndex.foreach { case ((r1, r2), i) => if (r1 != r2) println(i) }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = input.map(_.toCharArray)
    println(parsed)
    println(part1(parsed))
    println(part2(parsed))
    test(parsed)
