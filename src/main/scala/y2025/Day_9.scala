package com.cannondev.advent
package y2025

import util.FileReader

import scala.io.Source

object Day_9:
  case class Tile(x: BigInt, y: BigInt)

  val index = 9

  def part1(tiles: List[Tile]) = {
    val sizes = for {
      t1 <- tiles
      t2 <- tiles
    } yield {
      val x = t1.x - t2.x + 1
      val y = t1.y - t2.y + 1
      (x * y).abs
    }
    sizes.max
  }

  def parse(lines: List[String]): List[Tile] =
    lines.map {
      case s"$y,$x" => Tile(x.toInt, y.toInt)
      case s        => throw new Error(s"invalid input $s")
    }

  // 2147470248, 4761736832
  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
