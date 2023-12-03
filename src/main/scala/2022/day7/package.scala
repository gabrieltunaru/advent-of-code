package object day7 {

  sealed trait Tree

  case class Branch(name: String, parents: List[String] = Nil) extends Tree

  case class Leaf(name: String, size: Int, parents: List[String] = Nil) extends Tree

  enum FileType:
    case File, Dir

  case class FileInfo(fileType: FileType, size: Int)
}
