package com.cannondev.advent

import util.FileReader

import scala.annotation.tailrec
import scala.io.Source

object Day_5:
  case class Input(intervals: List[Interval], values: List[BigInt])
  case class Interval(start: BigInt, stop: BigInt)

  val index = 5

  def parse(lines: String): Input =
    val split = lines.split("\n\n")
    val intervals = split.head
      .split("\n")
      .map {
        case s"$a-$b" => Interval(BigInt(a), BigInt(b))
        case s        => throw new Error(s"Invalid input for interval $s")
      }
    val values = split.tail.head.split("\n").map(x => BigInt(x))
    Input(intervals.toList, values.toList)

  def part1(input: Input): BigInt =
    input.values.count(v => input.intervals.exists(i => v >= i.start && v <= i.stop))

  def union(a: Interval, b: Interval): BigInt =
    val start = a.start.max(b.start)
    val stop = a.stop.min(b.stop)
    if (stop >= start) stop - start + 1
    else 0

  def mergeIntervals(a: Interval, b: Interval): Option[Interval] =
    if (b.start <= a.stop) Some(Interval(a.start, b.stop))
    else None

  def forceMerge(a: Interval, b: Interval): Interval = {
    val start = a.start.min(b.start)
    val stop = a.stop.max(b.stop)
    Interval(start, stop)
//    List(a, b).sortBy(_.start) match {
//      case x::y::Nil => Interval(a.start, b.stop)
//      case _ => throw new Error("should never happen")
//    }
  }

  def sort(input: List[Interval]): List[Interval] = {
    input match {
      case first :: second :: tail =>
        mergeIntervals(first, second) match {
          case Some(value) => sort(value :: tail)
          case None        => first :: sort(second :: tail)
        }
      case rest => rest
    }
  }

  def findFirst(input: List[Interval], remaining: List[Interval]): Option[(Interval, Interval)] =
    remaining match {
      case i :: tail =>
        input.find(j => i != j && j.start <= i.stop && j.start >= i.start) match {
          case Some(value) => Some(i, value)
          case None        => findFirst(input, tail)
        }
      case Nil => None
    }

  @tailrec
  def find(input: List[Interval]): List[Interval] = {
    val distinct = input.distinct
    findFirst(distinct, distinct) match {
      case Some((a, b)) =>
        val filtered = distinct.filterNot(i => i == a || i == b)
        find(forceMerge(a, b) :: filtered)
      case None => distinct
    }
  }

  // 336185947296474
  // 388342136956631 -> too high
  // 359526404143201
  // 359526404143201
  def part2(intervals: List[Interval]): BigInt =
    find(intervals.sortBy(_.start)).map(i => i.stop - i.start + 1).sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2025)
    val parsed = parse(input)
//    println(parsed)
    println(part1(parsed))
    println(part2(parsed.intervals))
//    println(sort(parsed.intervals.sortBy(_.start)))
