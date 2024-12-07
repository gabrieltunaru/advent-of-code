package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_5:
  case class Rule(x: Int, y: Int)
  case class Something(rules: List[Rule], printed: List[List[Int]])

  val index = 5

  def parse(lines: String): Something =

    val splitted = lines.split("\n\n")
    val rulesString = splitted(0)
    val rules = rulesString
      .split("\n")
      .map(s =>
        val splitted = s.split("\\|")
        Rule(splitted(0).toInt, splitted(1).toInt)
      )
      .toList

    val lists = splitted(1).split("\n").map(_.split(",").map(_.toInt).toList).toList
    Something(rules, lists)

  def part1(input: Something) =
    val filtered = input.printed.filter(l =>
      input.rules.forall(rule => {
        if (l.contains(rule.x) && l.contains(rule.y))
          l.indexOf(rule.x) < l.indexOf(rule.y)
        else true
      })
    )
    filtered

  def part2(input: Something, correctLists: List[List[Int]]) =
    val wrongLists = input.printed.filter(l => !correctLists.contains(l))
    println(wrongLists)
  def main(args: Array[String]): Unit =
    val input = FileReader.readString(index, 2024)
    val parsed = parse(input)
//    println(parsed)
    val correctLists = part1(parsed)
    val resPart1 = correctLists.map(l => l(l.length / 2)).sum
    println(resPart1)
    println(part2(parsed, correctLists))
