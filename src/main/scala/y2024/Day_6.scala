package com.cannondev.advent
package y2024

import util.FileReader

import scala.io.Source

object Day_6:
  case class RoomMap(obstacles: List[Position], start: Position, xMax: Int, yMax: Int)
  case class Position(line: Long, column: Long)

  enum GuardDirection {
    case Up
    case Down
    case Left
    case Right
  }

  sealed abstract class Cell(val x: Int, val y: Int)
  case class FreeSpace(override val x: Int, override val y: Int) extends Cell(x, y)
  case class Visited(override val x: Int, override val y: Int) extends Cell(x, y)
  case class Obstacle(override val x: Int, override val y: Int) extends Cell(x, y)
  case class Guard(override val x: Int, override val y: Int, direction: GuardDirection) extends Cell(x, y) {
    def rotate: Guard =
      Guard(
        this.x,
        this.y,
        direction match
          case GuardDirection.Up    => GuardDirection.Right
          case GuardDirection.Down  => GuardDirection.Left
          case GuardDirection.Left  => GuardDirection.Up
          case GuardDirection.Right => GuardDirection.Down
      )
  }
  object Guard {
    def parseDirection(char: Char) =
      char match
        case '^' => GuardDirection.Up
        case 'v' => GuardDirection.Down
        case '<' => GuardDirection.Left
        case '>' => GuardDirection.Right
        case c   => throw new Error(s"Invalid character $c")
  }

  def part1(map: Set[Cell], xMax: Int, yMax: Int): Set[Cell] =
    val guard = map.flatMap {
      case g: Guard => Some(g)
      case _        => None
    }.head
    val (xNext, yNext) = guard.direction match
      case GuardDirection.Up    => (guard.x - 1, guard.y)
      case GuardDirection.Down  => (guard.x + 1, guard.y)
      case GuardDirection.Left  => (guard.x, guard.y - 1)
      case GuardDirection.Right => (guard.x, guard.y + 1)
    if (xNext > xMax || yNext > yMax || xNext == 0 || yNext == 0) map
    else {
//      println(map)
//      println((xNext, yNext))
      val nextCell = map.find(c => c.x == xNext && c.y == yNext).get
      val nextMap = nextCell match
        case _: FreeSpace | _: Visited =>
          map.diff(Set(guard)) + Visited(guard.x, guard.y) + Guard(xNext, yNext, guard.direction)
        //        case _: Visited   => Visited(guard.x, guard.y) :: Guard(xNext, yNext, guard.direction) :: map.diff(List(guard))
        case _: Obstacle => map.diff(Set(guard)) + guard.rotate // TODO: add VisitedNearObstacle for part2
        case _: Guard    => throw new Error("there can't be 2 guards")
      part1(nextMap, xMax, yMax)
    }

  val index = 6

  def parse(lines: List[String]): Set[Cell] =

    val parsedMap = for
      i <- lines.indices
      j <- lines.head.indices
      parsed = lines(i)(j) match
        case '.' => FreeSpace(i, j)
        case '#' => Obstacle(i, j)
        case c   => Guard(i, j, Guard.parseDirection(c))
    yield parsed
    parsedMap.toSet

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
//    println(parsed)
    val xMax = parsed.map(_.x).max
    val yMax = parsed.map(_.y).max
    val res = part1(parsed, xMax, yMax)
//    println(res)
    println(Set.from(res).count {
      case _: Visited => true
      case _          => false
    } + 1)

    /**
     * tried: 5211, 5212
     **/