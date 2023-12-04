package com.cannondev.advent

import util.FileReader

import cats.kernel.Semigroup

import scala.io.Source

object Day_4:
  case class ScratchCard(winningNumbers: Set[Int], myNumbers: Set[Int])

  val index = 4

  def parse(lines: List[String]): List[ScratchCard] =
    lines.map(line => {
      val numbers = line.split(":")(1)
      val winningString = numbers.split('|')(0)
      val myString = numbers.split('|')(1)
      val winningNumbers = winningString.split(" +").filter(_.nonEmpty).map(_.toInt).toSet
      val myNumbers = myString.split(" +").filter(_.nonEmpty).map(_.toInt).toSet
      ScratchCard(winningNumbers, myNumbers)
    })

  def part1(cards: List[ScratchCard]): Int =
    val myWinningCards = cards.map(card => card.winningNumbers.intersect(card.myNumbers).size)
    val powered = myWinningCards.filter(_ > 0).map(nr => Math.pow(2, nr - 1))
    powered.map(_.toInt).sum

  def part2(cards: List[ScratchCard]): Int =
    val myWinningCards =
      cards.zipWithIndex.map((card, index) => (card.winningNumbers.intersect(card.myNumbers).size, index))

    val initialMap = cards.zipWithIndex.map((_, i) => i -> 1).toMap
    val res = myWinningCards.foldLeft(initialMap) { case (acc, (winningCards, index)) =>
      val currentCardCopies = acc.getOrElse(index, 0)

      val mapEntries = (1 to winningCards).map(_ + index).map(_ -> currentCardCopies).toMap
      Semigroup.combine(acc, mapEntries)
    }
    res.values.sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(part1(parsed))
    println(part2(parsed))
