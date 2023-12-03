import scala.io.Source

object Day8_2:
  val resources = "src/main/resources"
  val index = 8
  val filePath = s"$resources/day_$index.txt"

  def count(current: Int, slice: Array[Int]): Int =
    val firstBigger = slice.indexWhere(_>=current)
    val end = if (firstBigger == -1) slice.length else firstBigger
    val smallerSlice = slice.slice(0, end+1)
    smallerSlice.length

  def isVisible(x: Int, y: Int, viewMatrix: Array[Array[Int]]): Int =
    val leftSlice = viewMatrix(x).slice(0, y).reverse
    val rightSlice = viewMatrix(x).slice(y + 1, viewMatrix(x).length)
    val topSlice = viewMatrix.slice(0, x).map(_(y)).reverse
    val bottomSlice = viewMatrix.slice(x + 1, viewMatrix.length).map(_(y))

    val current = viewMatrix(x)(y)

    val res = List(leftSlice, rightSlice, topSlice, bottomSlice)
      .map(count(current,_)).product
    res

  def iterate(viewMatrix: Array[Array[Int]]): Seq[Seq[Int]] =
    val visibles = for {
      i <- viewMatrix.indices
      j <- viewMatrix(i).indices
    } yield isVisible(i, j, viewMatrix)
    visibles.grouped(viewMatrix.length).toSeq

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val input = fileContents.map(_.toCharArray.map(_.asDigit)).toArray
    val res = iterate(input).map(_.max).max
//    res.foreach(println)
    println(res)
