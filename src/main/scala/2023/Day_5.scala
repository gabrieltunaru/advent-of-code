package com.cannondev.advent

import util.FileReader

object Day_5:
  case class Input(seeds: List[BigInt], maps: List[List[PlantingMap]])
  case class PlantingMap(destinationRange: BigInt, sourceRange: BigInt, rangeLength: BigInt)

  val index = 5

  def parse(lines: String): Input =

    val separated =
      lines
        .filter(c => c == ' ' || c == '\n' || c.isDigit)
        .split("\n\n")

    val seeds = separated.head.split(' ').filter(_.nonEmpty).map(BigInt(_))
    val maps = separated.tail
      .map(_.split("\n").filter(_.nonEmpty).map(_.split(' ').map(BigInt(_))))
      .map(_.filter(_.nonEmpty).toList.map {
        case Array(a, b, c) => PlantingMap(a, b, c)
        case _              => throw new Error("could not match")
      })
      .toList

    Input(seeds.toList, maps)

  def part1(input: Input): BigInt =
    input.maps
      .foldLeft(input.seeds)((acc, maps) => {
        val res = acc.map(seed =>
          maps.find(map => seed >= map.sourceRange && seed < map.sourceRange + map.rangeLength) match
            case Some(foundMap) => seed - foundMap.sourceRange + foundMap.destinationRange
            case None           => seed
        )
        res
      })
      .min

  def part2(input: Day_5.Input): BigInt =
    val actualSeeds = input.seeds.grouped(2).toList.flatMap {
      case a :: b :: Nil => (a to a + b - 1).toList
      case _             => throw new Error
    }
    val actualInput = part1(input.copy(seeds = actualSeeds))

    actualInput
  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2023)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
    println(part2(parsed))
