package com.cannondev.advent
package y2025

import util.FileReader

import cats.Semigroup

import scala.io.Source
import cats.syntax.all.*

object Day_4:
  case class Position(x: Int, y: Int)
  object Position {
    given semigroup: Semigroup[Position] with
      override def combine (a: Position, b: Position): Position = Position(a.x+b.x, a.y+b.y)
  }

  val index = 4

  def parse(lines: List[String]): List[Position] =
    lines.zipWithIndex.flatMap((s,i) => s.toCharArray.zipWithIndex.flatMap((c,j) => if(c=='@') Some(Position(i,j)) else None))

  private val adiacentIndexedToAdd = for {
    i <- -1 to 1
    j <- -1 to 1
  } yield Position(i,j)

  def countNeighbours(input: List[Position]): List[Int] = {
    input.map(p => {
      val adiacent = adiacentIndexedToAdd.map(other => p |+| other)
      val neighbours = adiacent.count(other => input.contains(other))
      neighbours
    })
  }

  def part1(input: List[Position]) =
    val neighbours = countNeighbours(input)
    neighbours.count(_ <= 4)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
//    parsed.zip(countNeighbours(parsed)).foreach((p,n) => println(s"$p - $n"))
    println(part1(parsed))