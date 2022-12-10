package com.cannondev.advent

import scala.io.Source

case class State(cycle: Int, value: Int, lastLine: Option[String])

object Day10_1:
  val resources = "src/main/resources"
  val index = 10
  val filePath = s"$resources/day_$index.txt"

  def move(lines: List[String], states: List[State]): List[State] =

    val noop = "noop".r
    val addx = "addx (.*)".r

    val line = lines.headOption

    val state = states.head

    line.map {
      case noop() => move(lines.tail, State(state.cycle + 1, state.value, line) :: states)
      case addx(v) =>
        move(
          lines.tail,
          State(state.cycle + 2, state.value + v.toInt, line) :: State(state.cycle + 1, state.value, line) :: states
        )

      case e => throw new Error(s"invalid input: $e")
    } getOrElse states

  def getSignalStrength(state: State) =
    state.value * state.cycle

  def getSignalIntervals(cycles: Int): Seq[Int] =
    val length = cycles / 40
    if (cycles >= 20 && cycles < 40) List(20)
    else (0 until length).map(_ * 40).map(_ + 20)

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines().toList
    val initialStates = List(State(0, 1, None))
    val signals = move(fileContents, initialStates)
    val signalIntervals = getSignalIntervals(signals.length)
    val neededSignals = signalIntervals.flatMap(cycle => signals.find(_.cycle == cycle))
    val res = neededSignals.map(getSignalStrength)
    println(res)
    println(res.sum)
    val test = signals.filter(_.cycle >= 179)
    test.foreach(println)
