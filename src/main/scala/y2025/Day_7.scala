package com.cannondev.advent
package y2025

import util.FileReader

import com.cannondev.advent.y2024.Day_7.*
import com.cannondev.advent.y2025.Day_7.Symbol.Start

import scala.io.Source

object Day_7:
  enum Symbol {
    case Start
    case Split
    case Beam
  }

  case class Cell(x: Int, y:Int) {
    def goDown() = Cell(x+1, y)
    def split() = List(
      Cell(x, y-1),
      Cell(x, y+1)
    )
  }
  case class Input(
      splitters: List[Cell],
      tachyons: List[Cell]
  )
  case class Something(x: Int, y: Int, s: Symbol)

  val index = 7

  def parse(lines: List[String]): Input = {
    val raw = lines.zipWithIndex.flatMap((s, i) =>
      s.toCharArray.zipWithIndex
        .flatMap((c, j) => {
          val cell = c match {
            case 'S' => Some(Symbol.Start)
            case '^' => Some(Symbol.Split)
            case _ => None
          }
          cell.map(c => Something(i, j, c))
        })
        .toList
    )
    val start = raw.find(_.s == Start).get
    val splitters = raw.filter(_.s == Symbol.Split).map(x => Cell(x.x, x.y))
    Input(splitters, List(Cell(start.x, start.y)))
  }
  
  case class Accumulator(
      tachyons: List[Cell],
      count: Int
                        )

  def part1(input: Input, current: Int, max: Int, count: Int = 0): Int = {
    if(current>max) count
    else {
      val onThisRow = input.tachyons.filter(_.x == current)
      val next = onThisRow.map(_.goDown())
      val splitOrNot = next.foldLeft(Accumulator(Nil,0 ))((acc, el) => 
      if (input.splitters.contains(el)) Accumulator((el.split() ++ acc.tachyons).distinct, acc.count+1)
      else Accumulator(el :: acc.tachyons, acc.count)
      )
      part1(input.copy(tachyons = splitOrNot.tachyons), current+1, max, count + splitOrNot.count)
    }
  }

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2025)
    val parsed = parse(input)
    println(parsed)
    val res1 = part1(parsed, 0, input.length, 0)
    print(res1)
