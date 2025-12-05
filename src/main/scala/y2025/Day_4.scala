package com.cannondev.advent
package y2025

import util.FileReader

import cats.Semigroup

import scala.io.Source
import cats.syntax.all.*

import scala.annotation.tailrec

object Day_4:
  case class Position(x: Int, y: Int)
  object Position {
    given semigroup: Semigroup[Position] with
      override def combine(a: Position, b: Position): Position = Position(a.x + b.x, a.y + b.y)
  }

  case class PositionWithNeighbours(x: Int, y: Int, neighbours: Int)

  val index = 4

  def parse(lines: List[String]): List[Position] =
    lines.zipWithIndex.flatMap((s, i) =>
      s.toCharArray.zipWithIndex.flatMap((c, j) => if (c == '@') Some(Position(i, j)) else None)
    )

  private val adiacentIndexedToAdd = for {
    i <- -1 to 1
    j <- -1 to 1
  } yield Position(i, j)

  def countNeighbours(input: List[Position]): List[PositionWithNeighbours] = {
    input.map(p => {
      val adiacent = adiacentIndexedToAdd.map(other => p |+| other)
      val neighbours = adiacent.count(other => input.contains(other))
      PositionWithNeighbours(p.x, p.y, neighbours)
    })
  }

  def part1(input: List[Position]): Int =
    val neighbours = countNeighbours(input)
    neighbours.count(_.neighbours <= 4)

  @tailrec
  def part2(input: List[Position], removed: Int): Int =
    val neighbours = countNeighbours(input)
    val toRemove = neighbours.count(_.neighbours <= 4)
    val newInput = neighbours.filter(_.neighbours > 4).map(p => Position(p.x, p.y))
    if (toRemove == 0) removed + toRemove
    else part2(newInput, removed + toRemove)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(part1(parsed))
    println(part2(parsed, 0))
