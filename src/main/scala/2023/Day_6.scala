package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_6:
  case class Input(times: List[Long], distances: List[Long])

  val index = 6
  def parseLine(s: String): List[Long] =
    s
      .split(':')(1)
      .replaceAll(" +", ",")
      .split(',')
      .filter(_.nonEmpty)
      .map(_.toLong)
      .toList

  def parse(lines: List[String]): Input =
    val times = parseLine(lines(0))
    val distances = parseLine(lines(1))
    Input(times, distances)

  def part1(input: Input): Long =
    input.times
      .zip(input.distances)
      .map((maxTime, maxDistance) =>
        (1.toLong until maxTime)
          .map(pressedTime => pressedTime * (maxTime - pressedTime))
          .count(_ > maxDistance)
      )
      .product

  def concatenateNumbers(l: List[Long]): Long = l.map(_.toString).foldLeft("")(_ + _).toLong

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
    val part2Input = Input(List(concatenateNumbers(parsed.times)), List(concatenateNumbers(parsed.distances)))
    println(part2Input)
    println(part1(part2Input))
