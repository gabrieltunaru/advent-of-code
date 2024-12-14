package com.cannondev.advent

import util.FileReader

import scala.io.Source

object Day_14:
  case class Coordinate(x: Int, y: Int)
  case class Robot(position: Coordinate, xVelocity: Int, yVelocity: Int)
  case class Quadrant(xMin: Int, yMin: Int, xMax: Int, yMax: Int) {
    def contains(robot: Robot): Boolean =
      robot.position.x >= xMin && robot.position.x <= xMax &&
        robot.position.y >= yMin && robot.position.y <= yMax
  }

  val index = 14
  val xMax = 103
  val yMax = 101

  val firstQuadrant = Quadrant(0, 0, xMax / 2 - 1, yMax / 2 - 1)
  val secondQuadrant = Quadrant(xMax / 2 + 1, 0, xMax, yMax / 2 - 1)
  val thirdQuadrant = Quadrant(0, yMax / 2 + 1, xMax / 2 - 1, yMax)
  val fourthQuadrant = Quadrant(xMax / 2 + 1, yMax / 2 + 1, xMax, yMax)

  def parse(lines: List[String]): List[Robot] =

    val pattern = "p=(.*),(.*) v=(.*),(.*)".r

    lines.map {
      case pattern(y, x, yV, xV) => Robot(Coordinate(x.toInt, y.toInt), xV.toInt, yV.toInt)
      case e                     => throw new Error(s"Invalid input $e")
    }

  def moveRobot(robot: Robot): Robot =
    val newX = robot.position.x + robot.xVelocity
    val newY = robot.position.y + robot.yVelocity
    val sanitizedX =
      if (newX >= xMax) newX - xMax
      else if (newX < 0) newX + xMax
      else newX

    val sanitizedY =
      if (newY >= yMax) newY - yMax
      else if (newY < 0) newY + yMax
      else newY

    robot.copy(position = Coordinate(sanitizedX, sanitizedY))

  def part1(input: List[Robot]): BigInt =
    val moved = (1 to 100).foldLeft(input)((acc, _) => {
//      println(acc)
      acc.map(robot => moveRobot(robot))
    })
//    printRobots(moved)
//    println(moved.map(_.position))
    val first = moved.count(r => firstQuadrant.contains(r))
    val second = moved.count(r => secondQuadrant.contains(r))
    val third = moved.count(r => thirdQuadrant.contains(r))
    val fourth = moved.count(r => fourthQuadrant.contains(r))
//    printRobots(moved)
    BigInt(first) * BigInt(second) * BigInt(third) * BigInt(fourth)

  def printRobots(input: List[Robot]) =
    for {
      i <- 0 until xMax
      s = (0 until yMax).map(j =>
        val robots = input.count(r => r.position.y == j && r.position.x == i)
        if (robots > 0) robots.toString else "."
      ).mkString
      _ = println(s)
    } yield ()

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed)
    println(part1(parsed))
