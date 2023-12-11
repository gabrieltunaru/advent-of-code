package com.cannondev.advent

import util.FileReader

import scala.annotation.tailrec
import scala.io.Source

object Day_9:
  case class Something(value: Int)

  val index = 9

  @tailrec
  def getNext(l: List[Int], acc: Int): Int =
    val nextSequence = l.sliding(2).map { case a :: b :: Nil => b - a }.toList
    if (nextSequence.forall(_ == 0)) acc + l.last
    else
      getNext(nextSequence, l.last + acc)

  def getNextP2(l: List[Int]): Int =
    val nextSequence = l.sliding(2).map { case a :: b :: Nil => b - a }.toList
    if (nextSequence.forall(_ == 0)) l.head
    else
      val res = getNextP2(nextSequence)
      l.head - res


  def parse(lines: List[String]): List[List[Int]] =
    lines.map(_.split(' ').map(_.toInt).toList)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    val part1 = parsed.map(l => getNext(l, 0))
    println(part1.sum)
    val part2 = parsed.map(l => getNextP2(l))
    println(part2.sum)
