import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day6_1 {
  val resources = "src/main/resources"
  val index = 6
  val filePath = s"$resources/day_$index.txt"

  def findStart(s: String, size: Int): Int =
    val packsOf4 = s.sliding(size)
    val packsOf4WithIndex = packsOf4.zipWithIndex
    for ((value, index) <- packsOf4WithIndex) {
      val arr = value.toCharArray
      val set = Set.from(arr)
      if (arr.size == set.size) return index+size
    }
    -1

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).mkString
    println(findStart(fileContents,4)) // part 1
    println(findStart(fileContents,14)) // part 2
}
