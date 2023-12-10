package com.cannondev.advent

import util.FileReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source


object Day_7:
  case class Input(cards: List[Char], bidAmount: Int)

  case class CardsWithRank(cards: List[Char], bidAmount: Int, rank: Int)

  val cardRank = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

  val index = 7

  def parse(lines: List[String]): List[Input] =

    lines.map(line => line.split(" ").toList match
      case a :: b :: Nil => Input(a.toCharArray.toList, b.toInt)
      case _ => throw new Error
    )

  def determineRank(cards: List[Char]): Int =
    val cardsSet = cards.toSet
    cardsSet.toList.map(card => cards.count(otherCard => card == otherCard)).sorted.reverse match
      case 5 :: Nil => 7
      case 4 :: _ => 6
      case 3 :: 2 :: Nil => 5
      case 3 :: _ => 4
      case 2 :: 2 :: _ => 3
      case 2 :: _ => 2
      case _ => 1

  @tailrec
  def compare2(myCard: List[Char], otherCard: List[Char]): Int =
    (myCard, otherCard) match
      case (Nil, Nil) => 0
      case (a :: t1, b :: t2) if a == b => compare2(t1, t2)
      case (a :: _, b :: _) if cardRank.indexOf(a) > cardRank.indexOf(b) => 1
      case (a :: _, b :: _) if cardRank.indexOf(a) < cardRank.indexOf(b) => -1
      case _ => throw new Error(s"no idea: $myCard, $otherCard")

  def mapToSecondRank(cardsWithOneRank: List[CardsWithRank]) =
    val sorted = cardsWithOneRank.sorted((a, b) => compare2(a.cards, b.cards))
    val updated = sorted.zipWithIndex.map((cards, i) => cards.copy(rank = i + 1))
    updated

  def part1V2(input: List[Input]): Int =
    val cardsWithRank = input.map(i => CardsWithRank(i.cards, i.bidAmount, determineRank(i.cards)))
    val mapped = cardsWithRank.map(_.rank).distinct.sorted.map( rank => {
      val filtered = cardsWithRank.filter(_.rank == rank)
      val res = mapToSecondRank(filtered)
      res
    })
    println(s"mapped: $mapped")

    val res2 = mapped.foldLeft(List.empty[CardsWithRank])((acc, el) => {
      val updatedEl = el.map(c => c.copy(rank = c.rank + acc.size))
      acc ::: updatedEl
    })

    println(s"res2: $res2")

    res2.foreach(c => println(s"r: ${c.cards.foldLeft("")(_+_)} ${c.rank}, b: ${c.bidAmount}"))
    res2.map(c => c.rank * c.bidAmount).sum


  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2023)
    val parsed = parse(input)
    println(parsed.map(_.cards.foldLeft("")(_ + _)).toSet.size)
    println(parsed)
    println(part1V2(parsed))

