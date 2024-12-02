package com.cannondev.advent.y2024

import com.cannondev.advent.util.FileReader
import scala.io.Source

object Day_2:
  case class Something(value: Int)

  val index = 2

  def parse(lines: List[String]): List[List[Int]] =
    lines.map(_.split(" ").toList.map(_.toInt))

  private def isSafe(x: Int, y: Int, ascending: Boolean): Boolean =
    if (ascending) x > y && (x - y).abs >= 1 && (x - y).abs <= 3
    else x < y && (x - y).abs >= 1 && (x - y).abs <= 3

  def part1(in: List[List[Int]]): List[Int] =
    for {
      l <- in
      el1 = l.head
      el2 = l.tail.head
      isSafeList = l.tail.foldLeft((el1, true)) {
        case ((accEl, accIsSafeUntilNow), el) => {
          val isElSafe = accIsSafeUntilNow && isSafe(accEl, el, el1 > el2)
          (el, isElSafe)
        }
      }
    } yield if (isSafeList._2) 1 else 0

//  def part2(in: List[List[Int]]): List[Int] =
//    for {
//      l <- in
//      el1 = l.head
//      el2 = l.tail.head
//      isSafeList = l.tail.foldLeft((el1, true, 0)) {
//        case ((accEl, accIsSafeUntilNow, skips), el) => {
//          if (skips == 1) (el, accIsSafeUntilNow, skips)
//          else {
//            val isElSafe =  isSafe(accEl, el, el1 > el2)
//            if (!isElSafe && skips == 0) (el, accIsSafeUntilNow, 1)
//            else if (!isElSafe) (el, isElSafe, skips + 1)
//            else (el, accIsSafeUntilNow && isElSafe, skips)
//          }
//        }
//      }
//    } yield if (isSafeList._2) 1 else 0

  def checkList(l: List[Int], start: Int, isAscending: Boolean): Boolean =
    val res = l.tail.foldLeft((start, true)) {
      case ((accEl, accIsSafeUntilNow), el) => {
        val isElSafe = accIsSafeUntilNow && isSafe(accEl, el, isAscending)
        (el, isElSafe)
      }
    }
    res._2

  def part2(in: List[List[Int]]): List[Int] =
    for {
      l <- in
      updated = l :: l.indices.map(i => l.patch(i, Nil, 1)).toList

      isSafeList = updated.map(ll => {
        val el1 = ll.head
        val el2 = ll.tail.head
        checkList(ll, el1, el1 > el2)
      })
    } yield if (isSafeList.exists(x => x)) 1 else 0

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed).sum)
    println(part2(parsed).sum)
