import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day5_2 {
  val resources = "src/main/resources"
  val index = 5
  val filePath = s"$resources/day_$index.txt"

  def parse(s: String): (String, String) =
    s.split("\\n\\n") match
      case Array(l, r) => (l, r)
      case e           => throw new Error(s"invalid input: ${e.toString}")

  def cleanTop(s: String): String =
    s.replaceAll("    ", " [0]") // replace empty cell with 0 so we can save the contents in a matrix
      .replaceAll("\\[|\\]| ", "") // remove [ ] :space:

  def parseTop(s: String): Array[Array[Char]] =
    s.split("\\n")
      .map(_.toCharArray)
      .map(l => l.filter(c => c.isLetter || c == '0'))

  def parseBottom(s: String): Array[(Int, Int, Int)] =
    val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r

    s.split("\\n")
      .map(line =>
        val pattern(amount, source, target) = line
        (amount.toInt, source.toInt, target.toInt)
      )

  def transpose(initial: Array[Array[Char]]): Array[mutable.Stack[Char]] =
    initial.transpose.map(a => mutable.Stack.from(a))

  def moveCrates(stacks: Array[mutable.Stack[Char]], moves: Array[(Int, Int, Int)]): Array[mutable.Stack[Char]] =
    for (move <- moves) {
      val (amount, source, target) = move
      val popped = stacks(source - 1).take(amount)
      for (_ <- 1 to amount) {
        stacks(source - 1).pop()
      }
      stacks(target - 1).pushAll(popped.reverse)
    }
    stacks

  def getTopLetters(stacks: Array[mutable.Stack[Char]]): String =
    stacks.map(_.pop()).mkString

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).mkString
    val (top, bottom) = parse(fileContents)
    val topCleaned = cleanTop(top)
    println(topCleaned)
    val topParsed = parseTop(topCleaned)
    val topTransposed = transpose(topParsed)
    val transposedCleaned = topTransposed.map(_.filterNot(_ == '0'))
    val bottomParsed = parseBottom(bottom)

    val computed = moveCrates(transposedCleaned, bottomParsed)

    println(transposedCleaned.map(_.toList.toString()).mkString("Array(", ", ", ")"))

    println(getTopLetters(computed))
}
