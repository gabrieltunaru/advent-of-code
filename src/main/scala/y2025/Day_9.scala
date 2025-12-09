package com.cannondev.advent
package y2025

import util.FileReader

import scala.io.Source

object Day_9:
  case class Tile(x: BigInt, y: BigInt)

  val index = 9

  private def area(t1: Tile, t2: Tile): BigInt = {
    val x = t1.x - t2.x + 1
    val y = t1.y - t2.y + 1
    (x * y).abs
  }

  def part1(tiles: List[Tile]) = {
    val sizes = for {
      t1 <- tiles
      t2 <- tiles
    } yield {
      area(t1, t2)
    }
    sizes.max
  }

  def parse(lines: List[String]): List[Tile] =
    lines.map {
      case s"$y,$x" => Tile(x.toInt, y.toInt)
      case s        => throw new Error(s"invalid input $s")
    }

  def existsUpperLeft(tiles: List[Tile], a: Tile, b: Tile) =
    tiles.exists(x => x.x <= a.x && x.y <= a.y) &&
      tiles.exists(x => x.x >= b.x && x.y >= b.y)

  def existsUpperRight(tiles: List[Tile], a: Tile, b: Tile) =
    tiles.exists(x => x.x <= a.x && x.y >= a.y) &&
      tiles.exists(x => x.x >= b.x && x.y <= b.y)

  def part2(tiles: List[Tile]) = {
    //format: off
    /**
     * Like in part 2, we construct the Cartesian product
     * Then we filter: for every combination, we create the other 2 corners
     * To be valid, the other 2 corners must  be inside the form
     * To be inside, there has to be another red tile in its exterior
     * ex: for upper left corner, there has to be a tile that is upper and/or more to the left than the corner
     * */
    //format: on
    val cartesianProdct = for {
      t1 <- tiles
      t2 <- tiles
    } yield (t1, t2)
    val valid = cartesianProdct.filter((t1, t2) => {
      val tt1 = Tile(t1.x, t2.y)
      val tt2 = Tile(t2.x, t1.y)
      val filtered = tiles.filterNot(t => t == t1 || t == t2)
      existsUpperLeft(filtered, tt1, tt2) && ???
      || existsUpperRight(filtered, tt1, tt2) && ???
    })
    valid.map(area).max
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
    println(part2(parsed))
