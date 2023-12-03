package com.cannondev.advent

import util.FileReader

import cats.kernel.Semigroup

import scala.io.Source

object Day_2:
  case class Something(value: Int)

  val index = 2

  def parse(lines: List[String]): List[List[Map[String, Int]]] =

    val linePattern = "Game (.*): (.*)".r
    val gamePattern = " ?([0-9]+) (.*)".r

    val parsedLines = lines.map(line =>
      val linePattern(gameNumber, text) = line
      val gamesStrings = text.split(";")
      val res = gamesStrings.map(gameString =>
        gameString
          .split(",")
          .map(dice =>
            val gamePattern(diceNumber, diceColor) = dice
            diceColor -> diceNumber.toInt
          )
          .toMap
      )
//      (gameNumber.toInt, text)
      res.toList
    )
    parsedLines

  def part1(games: List[List[Map[String, Int]]]): Int =
    val max = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )
//    println(games)

    val validGames = games.zipWithIndex
      .filter { (game, gameNumber) =>
        game.forall(diceCombinations => diceCombinations.forall { (color, number) => number <= max(color) })
      }
      .map((_, gameNumber) => gameNumber + 1)

    println(validGames)

    validGames.sum

  def part2(games: List[List[Map[String, Int]]]): Int =
    val max = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )

    implicit val semigroup: Semigroup[Map[String, Int]] = new Semigroup[Map[String, Int]] {
      override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] =
        val keys = x.keys ++ y.keys
        keys.map(key => (key, x.getOrElse(key,0).max(y.getOrElse(key,0)))).toMap
    }

    val dicesPerGame = games.map { game =>
      game.foldLeft(Map.empty)(semigroup.combine)
    }

    val validGames = dicesPerGame.map(_.values.product)
    println(dicesPerGame)
//    println(validGames)

    validGames.sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
//    val parted1 = part1(parsed)
//    println(parted1)
    println(part2(parsed))
