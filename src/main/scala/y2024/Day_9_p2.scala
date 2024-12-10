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

  case class File(startId: Int, length: Int) extends Block

  case class FreeSpace(length: Int) extends Block

  val index = 9

  def expand(input: List[Int], isFreeSpace: Boolean, currentId: Int): List[Block] =
    input match {
      case Nil => Nil
      case head :: tail =>
        if (isFreeSpace) FreeSpace(head) :: expand(tail, !isFreeSpace, currentId)
        else
          File(currentId, head) :: expand(tail, !isFreeSpace, currentId + 1)
    }

//  def defrag(input: List[Block]): List[Block] = {
//    val withIndex = input.zipWithIndex
//    val freeSpace = withIndex.flatMap {
//      case b@(FreeSpace(_), _) => Some(b)
//      case _ => None
//    }
//    val files = withIndex.reverse.flatMap {
//      case b@(File(_,_), _) => Some(b)
//      case _ => None
//    }
//    //    println(s"\nFreeSpace: ${freeSpace.mkString}")
//    //    println(s"\nFiles: ${files.mkString}")
//    val swapped: List[(Block, Int)] =
//      files.map(f => )
//    val remaining = withIndex.filter((_, i) => !swapped.exists((_, j) => i == j))
//    val all = remaining ++ swapped
//    val sorted = all.sortBy((_, i) => i)
//    sorted.map(_._1)
//
//  }

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
    expand(l, false, 0)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index, 2024)
    val parsed = parse(input)
    println(parsed.mkString)
    println(defrag(parsed).mkString)
//    println(part1(parsed))
