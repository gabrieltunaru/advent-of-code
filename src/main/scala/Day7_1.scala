import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait Tree

case class Branch(name: String, leafs: List[Tree]) extends Tree
case class Leaf(name: String, size: Int) extends Tree

object Day7_1 {
  val resources = "src/main/resources"
  val index = 7
  val filePath = s"$resources/day_$index.txt"

  def firstPass(lines: List[String], treeList: List[Tree], currentDir: mutable.Stack[String]): List[Tree] =

    val cd = "\\$ cd (.*)".r
    val ls = "\\$ ls".r
    val dir = "dir (.*)".r
    val file = "([0-9]+) (.+)".r

    val line = lines.headOption

    line
      .map {
        case cd("/") => firstPass(lines.tail, treeList, mutable.Stack.from(List("/")))
        case cd("..") => firstPass(lines.tail, treeList, currentDir.tail)
        case cd(d)   => firstPass(lines.tail, treeList, currentDir.push(d))
        case ls()    => firstPass(lines.tail, treeList, currentDir)
        case dir(d)  => firstPass(lines.tail, treeList.appended(Branch(d, Nil)), currentDir)
        case file(size, name) =>
          firstPass(lines.tail, treeList.appended(Leaf(name, size.toInt)), currentDir)
      }
      .getOrElse(treeList)

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val root: Tree = Branch("/", Nil)
    val currentDir = mutable.Stack.from(List("/"))
    val treeList = firstPass(fileContents.toList, List(root), currentDir)
    println(treeList)
}
