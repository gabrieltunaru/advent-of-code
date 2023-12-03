package com.cannondev.advent

import scala.collection.immutable.Set
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day4_3 {
  val resources = "src/main/resources"
  val index = 4
  val filePath = s"$resources/day_$index.txt"

  def getRange(s: String): List[Int] =
    s.split("-") match
      case Array(l, r) => List.range(l.toInt, r.toInt + 1)
      case _ => throw new Error("invalid input")

  def parse(s: String): (List[Int], List[Int]) =
    s.split(",") match
      case Array(l, r) => (getRange(l), getRange(r))
      case _ => throw new Error("invalid input")

  def areSubsets(first: List[Int], second: List[Int]): Boolean =

    val r1 = first.toSet
    val r2 = second.toSet

    r1.subsetOf(r2) || r2.subsetOf(r1)

  def areOverlapping(l: List[(List[Int],List[Int])]): Boolean =
    val (l11, l12) = l(0)
    val (l21, l22) = l(1)
    val s1 = l11.appendedAll(l12)
    val s2 = l21.appendedAll(l22)
    s1.intersect(s2).nonEmpty

  def flattenToSet(l: (List[Int],List[Int])): Set[Int] =
    val (l1, l2) = l
    l1.appendedAll(l2).toSet

  def countIntersections(l: List[Set[Int]]): Int =
    l match
      case Nil => 0
      case h::t => t.count(s => s.intersect(h).nonEmpty) + countIntersections(t)

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines.toList
    val parsed: List[(List[Int], List[Int])] = fileContents.map(parse)
    val sets = parsed.map(flattenToSet)
//    val operalsNo = sets.foldLeft(List.empty[Int])((acc, el, tail) =>
//      val no = sets.filter(s => s.intersect(el).nonEmpty)
//      no.size :: acc
//    )
//    println(operalsNo)
    println(countIntersections(sets))

}
