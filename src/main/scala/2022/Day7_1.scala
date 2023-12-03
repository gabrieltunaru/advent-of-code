import scala.io.Source

import day7.*
object Day7_1:
  val resources = "src/main/resources"
  val index = 7
  val filePath = s"$resources/day_$index.txt"

  def createPathString(pwd: List[String], name: String) =
    pwd.reverse.appended(name).mkString("/", "/", "")

  def parse(lines: List[String], treeList: List[Tree], pwd: List[String]): List[Tree] =

    val cd = "\\$ cd (.*)".r
    val ls = "\\$ ls".r
    val dir = "dir (.*)".r
    val file = "([0-9]+) (.+)".r

    val line = lines.headOption

    line
      .map {
        case cd("/")   => parse(lines.tail, treeList, List("/"))
        case cd("..")  => parse(lines.tail, treeList, pwd.tail)
        case cd(d)     => parse(lines.tail, treeList, d :: pwd)
        case ls()      => parse(lines.tail, treeList, pwd)
        case dir(name) => parse(lines.tail, treeList.appended(Branch(createPathString(pwd, name), pwd)), pwd)
        case file(size, name) =>
          parse(lines.tail, treeList.appended(Leaf(createPathString(pwd, name), size.toInt, pwd)), pwd)
      }
      .getOrElse(treeList)

  def updateParents(size: Int, parents: List[String], initial: Map[String, FileInfo]): Map[String, FileInfo] =

    val parentsPaths = parents.reverse
      .scanLeft(List[String]())((acc, elem) => elem :: acc)
      .map(_.reverse)
      .filterNot(_.isEmpty)
      .map(_.mkString("/", "/", ""))
    parentsPaths.foldRight(initial)((el, acc) =>
      val oldSize = acc.get(el).map(_.size).getOrElse(0)
      acc.updated(el, FileInfo(FileType.Dir, oldSize + size))
    )

  def main(args: Array[String]): Unit =
    val fileContents = Source.fromFile(filePath).getLines()
    val root: Tree = Branch("/", Nil)
    val currentDir = List("/")
    val treeList = parse(fileContents.toList, List(root), currentDir)
    val sizeMap = treeList.foldLeft(Map.empty[String, FileInfo])((acc, el) =>
      el match
        case Branch(_, _) => acc
        case Leaf(name, size, parents) =>
          updateParents(size, parents, acc.updated(name, FileInfo(FileType.File, size)))
    )
    val dirsLessThan100k =
      sizeMap.filter((_, fileInfo) => fileInfo.size <= 100000 && fileInfo.fileType == FileType.Dir)
    val sumOfSmallDirs = dirsLessThan100k.foldLeft(0)((acc, el) => acc + el._2.size)
    println(sumOfSmallDirs)

