import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait Tree

case class Branch(name: String, parents: List[String]= Nil) extends Tree
case class Leaf(name: String, size: Int, parents: List[String]= Nil) extends Tree

object Day7_1 {
  val resources = "src/main/resources"
  val index = 7
  val filePath = s"$resources/day_$index.txt"

  def firstPass(lines: List[String], treeList: List[Tree], pwd: mutable.Stack[String]): List[Tree] =

    val cd = "\\$ cd (.*)".r
    val ls = "\\$ ls".r
    val dir = "dir (.*)".r
    val file = "([0-9]+) (.+)".r

    val line = lines.headOption

    line
      .map {
        case cd("/") => firstPass(lines.tail, treeList, mutable.Stack.from(List("/")))
        case cd("..") => firstPass(lines.tail, treeList, pwd.tail)
        case cd(d)   => firstPass(lines.tail, treeList, pwd.push(d))
        case ls()    => firstPass(lines.tail, treeList, pwd)
        case dir(d)  => firstPass(lines.tail, treeList.appended(Branch(d, pwd.toList)), pwd)
        case file(size, name) =>
          firstPass(lines.tail, treeList.appended(Leaf(name, size.toInt, pwd.toList)), pwd)
      }
      .getOrElse(treeList)

  def updateParents(size: Int, parents :List[String], initial: Map[String, Int]): Map[String,Int] =
    parents.foldLeft(initial)((acc, el) =>
      val oldSize = acc.getOrElse(el, 0)
      acc.updated(el, oldSize + size)
    )

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val root: Tree = Branch("/", Nil)
    val currentDir = mutable.Stack.from(List("/"))
    val treeList = firstPass(fileContents.toList, List(root), currentDir)
    val sizeMap = treeList.foldLeft(Map.empty[String, Int])((acc, el) =>
      el match
        case Branch(name, _) => acc.updated(name,0)
        case Leaf(name, size, parents) => updateParents(size, parents, acc.updated(name,size))
    )
    val lessThan100k = sizeMap.filter((_, size) => size < 100000)
    val sumOfSmallOnes = lessThan100k.foldLeft(0)((acc, el) => acc + el._2)
    println(lessThan100k)
    println(sumOfSmallOnes)
}
