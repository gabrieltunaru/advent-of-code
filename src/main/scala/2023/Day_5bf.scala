package com.cannondev.advent

import util.FileReader

object Day_5bf:
  case class Input(seeds: List[Long], maps: List[List[PlantingMap]])

  case class PlantingMap(destinationRange: Long, sourceRange: Long, rangeLength: Long)

  val index = 5

  def parse(lines: String): Input =

    val separated =
      lines
        .filter(c => c == ' ' || c == '\n' || c.isDigit)
        .split("\n\n")

    val seeds = separated.head.split(' ').filter(_.nonEmpty).map(_.toLong)
    val maps = separated.tail
      .map(_.split("\n").filter(_.nonEmpty).map(_.split(' ').map(_.toLong)))
      .map(_.filter(_.nonEmpty).toList.map {
        case Array(a, b, c) => PlantingMap(a, b, c)
        case _              => throw new Error("could not match")
      })
      .toList

    Input(seeds.toList, maps)

  def part1(input: Input): Long =
    input.maps
      .foldLeft(input.seeds)((acc, maps) => {
        acc.map(seed =>
          maps.find(map => seed >= map.sourceRange && seed < map.sourceRange + map.rangeLength) match
            case Some(foundMap) => seed - foundMap.sourceRange + foundMap.destinationRange
            case None           => seed
        )
      })
      .min

  def compile(input: Input, seed: Long): Long =
    val res = input.maps.foldLeft(seed)((acc, maps) => {
      maps.find(map => acc >= map.sourceRange && acc < map.sourceRange + map.rangeLength) match
        case Some(foundMap) =>
          acc - foundMap.sourceRange + foundMap.destinationRange
        case None => acc
    })
    res

  def part2(input: Input): Long =
    val actualSeeds = input.seeds.grouped(2).toList.map {
      case a :: b :: Nil =>
        var min = Long.MaxValue
        for (i <- a until a + b) {
          val r = compile(input, i)
          if (r < min) min = r
        }
        min
      case _ => throw new Error
    }
    actualSeeds.min

  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2023)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
    println(part2(parsed))
