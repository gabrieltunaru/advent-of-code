package com.cannondev.advent

import util.FileReader

import scala.annotation.tailrec
import scala.io.Source

object Day_9:
  case class Something(value: Int)

  val index = 9

  @tailrec
  def getNext(l: List[Int], acc: Int): Int =
//    println(s"l: $l")
    val nextSequence = l.sliding(2).map { case a :: b :: Nil => b - a }.toList
//    println(s"nextSequence: $nextSequence")
//    println(s"acc: $acc")
    if (nextSequence.forall(_ == 0)) acc + l.last
    else
//      val nextAcc = l.zipWithIndex.map((a, b) => a + acc.lift(b).getOrElse(0))
//      println(s"nextAcc: $nextAcc")
      getNext(nextSequence, l.last + acc)
//    nextSequence.last

  def parse(lines: List[String]): List[List[Int]] =
    lines.map(_.split(' ').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val part1 = parsed.map(l => getNext(l, 0))
//    println(getNext(parsed.tail.head, 0))
    println(part1.sum)
