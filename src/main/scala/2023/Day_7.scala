package com.cannondev.advent

import util.FileReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day_7:
  case class Input(cards: List[Char], bidAmount: Int)

  case class CardsWithRank(cards: List[Char], bidAmount: Int, rank: Int)

  val index = 7

  def parse(lines: List[String]): List[Input] =
    lines.map(line =>
      line.split(" ").toList match
        case a :: b :: Nil => Input(a.toCharArray.toList, b.toInt)
        case _             => throw new Error
    )

  def determineRank(cards: List[Char]): Int =
    val cardsSet = cards.toSet
    cardsSet.toList.map(card => cards.count(otherCard => card == otherCard)).sorted.reverse match
      case 5 :: Nil      => 7
      case 4 :: _        => 6
      case 3 :: 2 :: Nil => 5
      case 3 :: _        => 4
      case 2 :: 2 :: _   => 3
      case 2 :: _        => 2
      case _             => 1

  val cardRankP2 = List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
  def determineRankP2(cards: List[Char]): Int =

    val counted = cards.distinct.filter(_ != 'J').map(card => cards.count(otherCard => card == otherCard))
    val jCount = cards.count(c => c == 'J')
//    val updated = counted.map(_ + jCount)
//    val allSorted = (jCount :: updated).sorted.reverse
    val allSorted = counted.sorted.reverse

    allSorted match
      case _ if jCount == 5                 => 7
      case x :: Nil if (x + jCount == 5)    => 7
      case x :: _ if x + jCount == 4        => 6
      case x :: 2 :: Nil if x + jCount == 3 => 5
      case x :: _ if x + jCount == 3        => 4
      case 2 :: 2 :: _                      => 3
      case x :: _ if (x + jCount == 2)      => 2
      case _                                => 1

  @tailrec
  def compare2(myCard: List[Char], otherCard: List[Char], cardRanks: List[Char]): Int =
    (myCard, otherCard) match
      case (Nil, Nil)                                                      => 0
      case (a :: t1, b :: t2) if a == b                                    => compare2(t1, t2, cardRanks)
      case (a :: _, b :: _) if cardRanks.indexOf(a) > cardRanks.indexOf(b) => 1
      case (a :: _, b :: _) if cardRanks.indexOf(a) < cardRanks.indexOf(b) => -1
      case _ => throw new Error(s"no idea: $myCard, $otherCard")

  def mapToSecondRank(cardsWithOneRank: List[CardsWithRank], cardRanks: List[Char]) =
    val sorted = cardsWithOneRank.sorted((a, b) => compare2(a.cards, b.cards, cardRanks))
    val updated = sorted.zipWithIndex.map((cards, i) => cards.copy(rank = i + 1))
    updated

  def part1V2(input: List[Input]): Int =
    val cardRank = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    val cardsWithRank = input.map(i => CardsWithRank(i.cards, i.bidAmount, determineRank(i.cards)))
    val mapped = cardsWithRank
      .map(_.rank)
      .distinct
      .sorted
      .map(rank => {
        val filtered = cardsWithRank.filter(_.rank == rank)
        val res = mapToSecondRank(filtered, cardRank)
        res
      })

    val res2 = mapped.foldLeft(List.empty[CardsWithRank])((acc, el) => {
      val updatedEl = el.map(c => c.copy(rank = c.rank + acc.size))
      acc ::: updatedEl
    })

    res2.foreach(c => println(s"r: ${c.cards.foldLeft("")(_ + _)} ${c.rank}, b: ${c.bidAmount}"))
    res2.map(c => c.rank * c.bidAmount).sum

  def part2(input: List[Input]): Int =
    val cardsWithRank = input.map(i => CardsWithRank(i.cards, i.bidAmount, determineRankP2(i.cards)))
    val mapped = cardsWithRank
      .map(_.rank)
      .distinct
      .sorted
      .map(rank => {
        val filtered = cardsWithRank.filter(_.rank == rank)
        val res = mapToSecondRank(filtered, cardRankP2)
        res
      })

    val res2 = mapped.foldLeft(List.empty[CardsWithRank])((acc, el) => {
      val updatedEl = el.map(c => c.copy(rank = c.rank + acc.size))
      acc ::: updatedEl
    })

    res2.foreach(c => println(s"r: ${c.cards.foldLeft("")(_ + _)} ${c.rank}, b: ${c.bidAmount}"))
    res2.map(c => c.rank * c.bidAmount).sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed.map(_.cards.foldLeft("")(_ + _)).toSet.size)
    println(parsed)
//    println(part1V2(parsed))
    println(part2(parsed))
