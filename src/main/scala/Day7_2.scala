import scala.io.Source

import day7.*


object Day7_2:
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


    val rootSize = sizeMap.get("//").map(_.size).getOrElse(0)
    val usedSpace = 70000000 - rootSize
    val sizeToDelete = 30000000 - usedSpace
    val sortedDirs = sizeMap.toList
      .filter(_._2.fileType == FileType.Dir)
      .sorted((a, b) => a._2.size.compareTo(b._2.size))

    println(rootSize)
    println(sizeToDelete)
    sortedDirs.foreach(println)

    val smallestNeededToDelete = sortedDirs.find { case (s, i) =>
      i.size >= sizeToDelete
    }

    println(smallestNeededToDelete)
