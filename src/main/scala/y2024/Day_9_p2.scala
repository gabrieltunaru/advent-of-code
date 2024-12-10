package com.cannondev.advent
package y2024

import util.FileReader

import scala.annotation.tailrec
import scala.io.Source

object Day_9_p2:
  case class Something(value: Int)

  sealed trait Block {
    def length: Int
    override def toString: String =
      this match
        case File(id, length)  => s"$id" * length
        case FreeSpace(length) => "." * length
  }

  case class File(id: Int, length: Int) extends Block

  case class FreeSpace(length: Int) extends Block

  val index = 9

  @tailrec
  def expand(input: List[Int], isFreeSpace: Boolean, currentId: Int, acc: List[Block]): List[Block] =
    input match {
      case Nil => acc
      case head :: tail =>
        if (isFreeSpace) expand(tail, !isFreeSpace, currentId, FreeSpace(head) :: acc)
        else
          expand(tail, !isFreeSpace, currentId + 1, File(currentId, head) :: acc)
    }

  def defrag(input: List[Block]): List[Block] =
    val withIndex = input.zipWithIndex
    val freeSpace = withIndex.flatMap {
      case b @ (FreeSpace(_), _) => Some(b)
      case _                     => None
    }
    val files = withIndex.reverse.flatMap {
      case b @ (File(_, _), _) => Some(b)
      case _                   => None
    }
    val fileToSwap = files.find((file, fileIndex) =>
      freeSpace.exists((freeSpace, freeSpaceIndex) => file.length <= freeSpace.length && freeSpaceIndex <= fileIndex)
    )
    fileToSwap match
      case Some(file) => {
        val spaceToSwap = freeSpace
          .find((freeSpace, freeSpaceIndex) => file._1.length <= freeSpace.length && freeSpaceIndex <= file._2)
          .get
        val spaceRemaining = spaceToSwap._1.length - file._1.length
        val newFile = (file._1, spaceToSwap._2)
        val newSpace = (spaceToSwap._1, file._2)
        if (spaceRemaining == 0) {
          val newInput = input.updated(newFile._2, newFile._1).updated(newSpace._2, newSpace._1)
//          val res = newInput.mkString
//          println(res)
          defrag(newInput)
        } else {
          val newInput = input
            .updated(newFile._2, newFile._1)
            .updated(newSpace._2, FreeSpace(newSpace._1.length - spaceRemaining))

          val reinsertedFreeSpace =
            newInput.take(newFile._2 + 1) ++ List(FreeSpace(spaceRemaining)) ++ newInput.drop(
              newFile._2 + 1
            ) // add remaining free space after file
//          val res = reinsertedFreeSpace.mkString
//          println(res)
          defrag(reinsertedFreeSpace)
        }
      }
      case None => input

  def parse(lines: List[String]): List[Block] =
    val l = lines.head.toCharArray.map(c => s"$c".toInt).toList
    //    println(l)
    expand(l, false, 0, Nil).reverse

  def part1(input: List[Block]): BigInt =
    val defragged = defrag(input)
    val expanded = defragged.flatMap(f => List.fill(f.length)(f))
//    expanded.zipWithIndex.foreach(x => print(x._2))
//    println("")
//    println(defragged.mkString)
//    expanded.zipWithIndex.foreach(x => print(x._1))
//    println("")
//    expanded.zipWithIndex.foreach((f, i) => println(s"$i * ${f.id}"))
//    expanded.zipWithIndex.map((f, i) => f.id * i).foldLeft(BigInt(0))(_ + _)

    expanded.zipWithIndex
      .map((f, i) =>
        f match
          case File(id, length)  => id * i
          case FreeSpace(length) => 0
      )
      .foldLeft(BigInt(0))(_ + _)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed.mkString)
//    println(defrag(parsed).mkString)
    println(part1(parsed))
