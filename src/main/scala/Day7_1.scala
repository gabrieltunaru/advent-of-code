import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait Tree

case class Branch(name: String, parents: List[String] = Nil) extends Tree
case class Leaf(name: String, size: Int, parents: List[String] = Nil) extends Tree

enum FileType:
  case File, Dir

case class FileInfo(fileType: FileType, size: Int)

object Day7_1 {
  val resources = "src/main/resources"
  val index = 7
  val filePath = s"$resources/day_$index.txt"

  def createPathString(pwd: mutable.Stack[String], name: String) =
    pwd.reverse.toList.appended(name).mkString("/","/","")

  def firstPass(lines: List[String], treeList: List[Tree], pwd: mutable.Stack[String]): List[Tree] =

    val cd = "\\$ cd (.*)".r
    val ls = "\\$ ls".r
    val dir = "dir (.*)".r
    val file = "([0-9]+) (.+)".r

    val line = lines.headOption

    line
      .map {
        case cd("/")  => firstPass(lines.tail, treeList, mutable.Stack.from(List.empty[String]))
        case cd("..") => firstPass(lines.tail, treeList, pwd.tail)
        case cd(d)    => firstPass(lines.tail, treeList, pwd.push(d))
        case ls()     => firstPass(lines.tail, treeList, pwd)
        case dir(name)   => firstPass(lines.tail, treeList.appended(Branch(createPathString(pwd, name), pwd.toList)), pwd)
        case file(size, name) =>
          firstPass(lines.tail, treeList.appended(Leaf(createPathString(pwd, name), size.toInt, pwd.toList)), pwd)
      }
      .getOrElse(treeList)

  def updateParents(size: Int, parents: List[String], initial: Map[String, FileInfo]): Map[String, FileInfo] =
    parents.foldLeft(initial)((acc, el) =>
      val index = parents.indexOf(el)
      val slice = parents.slice(index+1, parents.size)
      val path = createPathString(mutable.Stack.from(slice), el)
      val oldSize = acc.getOrElse(path, FileInfo(FileType.Dir, 0)).size
      acc.updated(path, FileInfo(FileType.Dir, oldSize + size))
    )

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val root: Tree = Branch("/", Nil)
    val currentDir = mutable.Stack.from(List.empty[String])
    val treeList = firstPass(fileContents.toList, List(root), currentDir)
    val sizeMap = treeList.foldLeft(Map.empty[String, FileInfo])((acc, el) =>
      el match
        case Branch(name, _)           => acc.updated(name, FileInfo(FileType.Dir, 0))
        case Leaf(name, size, parents) => updateParents(size, parents, acc.updated(name, FileInfo(FileType.File, size)))
    )
    val dirsLessThan100k = sizeMap.filter((_, fileInfo) => fileInfo.size < 100000 && fileInfo.fileType == FileType.Dir)
    val sumOfSmallDirs = dirsLessThan100k.foldLeft(0)((acc, el) => acc + el._2.size)
//    println(dirsLessThan100k)
    println(sumOfSmallDirs)
    sizeMap.toList.filter(_._2.fileType == FileType.Dir).sorted((a, b) => a._1.compareTo(b._1)).foreach(println)
    sizeMap.toList.filter(_._2.fileType == FileType.File).sorted((a, b) => a._1.compareTo(b._1)).foreach(println)

}
