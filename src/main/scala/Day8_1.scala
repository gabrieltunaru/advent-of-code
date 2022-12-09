import scala.io.Source

object Day8_1:
  val resources = "src/main/resources"
  val index = 8
  val filePath = s"$resources/day_$index.txt"

  def isVisible(x: Int, y: Int, viewMatrix: Array[Array[Int]]): Boolean =
    val leftSlice = viewMatrix(x).slice(0, y)
    val rightSlice = viewMatrix(x).slice(y+1, viewMatrix(x).length)
    val topSlice = viewMatrix.slice(0, x).map(_(y))
    val bottomSlice = viewMatrix.slice(x+1, viewMatrix.length).map(_(y))

    val current = viewMatrix(x)(y)

    List(leftSlice, rightSlice, topSlice, bottomSlice).exists(_.forall(_ < current))

  def iterate(viewMatrix: Array[Array[Int]]): Int =
    val visibles = for {
      i <- viewMatrix.indices
      j <- viewMatrix(i).indices
    } yield isVisible(i, j, viewMatrix)
    visibles.map(if (_) 1 else 0).sum

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val input = fileContents.map(_.toCharArray.map(_.asDigit)).toArray
    val res = iterate(input)
    println(res)
