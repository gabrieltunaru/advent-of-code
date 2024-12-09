package com.cannondev.advent
package y2024

import com.cannondev.advent.util.FileReader

import scala.io.Source

object Day_9:
  case class Something(value: Int)
  sealed trait Block {
    override def toString: String =
      this match
        case File(id)  => s"$id"
        case FreeSpace => "."
  }
  case class File(id: Int) extends Block
  case object FreeSpace extends Block

  val index = 9

  def expand(input: List[Int], isFreeSpace: Boolean, currentId: Int): List[Block] =
    input match {
      case Nil => Nil
      case head :: tail =>
        if (isFreeSpace) List.fill(head)(FreeSpace) ++ expand(tail, !isFreeSpace, currentId)
        else List.fill(head)(File(currentId)) ++ expand(tail, !isFreeSpace, currentId + 1)
    }

  def parse(lines: List[String]): List[Block] =
    val l = lines.head.toCharArray.map(c => s"$c".toInt).toList
    println(l)
    expand(l, false, 0)

  def defrag(input: List[Block]): List[Block] = {
    val withIndex = input.zipWithIndex
    val freeSpace = withIndex.flatMap {
      case b @ (FreeSpace, _) => Some(b)
      case _                  => None
    }
    val files = withIndex.reverse.flatMap {
      case b @ (File(_), _) => Some(b)
      case _                => None
    }
//    println(s"\nFreeSpace: ${freeSpace.mkString}")
//    println(s"\nFiles: ${files.mkString}")
    val swapped: List[(Block, Int)] =
      freeSpace.zip(files).flatMap { case ((freeSpace, freeSpaceIndex), (file, fileIndex)) =>
//        println(s"Swapped ($freeSpace, $freeSpaceIndex) with  ($file, $fileIndex) ")
        if (freeSpaceIndex < fileIndex)
          (freeSpace, fileIndex) :: (file, freeSpaceIndex) :: Nil
        else (freeSpace, freeSpaceIndex) :: (file, fileIndex) :: Nil
      }
    val remaining = withIndex.filter((_, i) => !swapped.exists((_, j) => i == j))
    val all = remaining ++ swapped
    val sorted = all.sortBy((_, i) => i)
    sorted.map(_._1)

  }

  def part1(input: List[Block]): Int =
    val defragged = defrag(input)
    val filtered = defragged.flatMap {
      case f: File => Some(f)
      case _       => None
    }
//    filtered.zipWithIndex.foreach((f, i) => println(s"$i * ${f.id}"))
//    filtered.zipWithIndex.foreach((f, i) => print(f.id))
//    println("")
    filtered.zipWithIndex.map((f, i) => f.id * i).sum

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed.mkString)
    println(part1(parsed))
