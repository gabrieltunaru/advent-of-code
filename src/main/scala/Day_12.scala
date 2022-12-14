package com.cannondev.advent

import util.FileReader

import scala.collection.mutable
import scala.io.Source

object Day_12:
  case class Point(x: Int, y: Int)
  type Something = List[List[Int]]
  case class Graph(nodes: Set[Point], edges: List[(Point, Point)]):
    self =>
    def ++(graph: Graph): Graph = Graph(self.nodes ++ graph.nodes, self.edges ++ graph.edges)

  object Graph:
    def empty = Graph(Set.empty, List.empty)

  val index = 12

  def getElevation(point: Point, matrix: List[List[Char]]): Char =
    matrix(point.x)(point.y) match
      case 'S' => 'a'
      case 'E' => 'z'
      case c   => c

  def isValid(curr: Point, next: Point, matrix: List[List[Char]]): Boolean =
    val currElevation = getElevation(curr, matrix)
    val nextElevation = getElevation(next, matrix)
    val res = nextElevation <= currElevation || currElevation + 1 == nextElevation
    println(s"${matrix(curr.x)(curr.y)} ${matrix(next.x)(next.y)} $res")
    res


  def convertToGraph(
      matrix: List[List[Char]]
  ): Graph =

    val nodes = for
      i <- matrix.indices
      j <- matrix.head.indices
    yield Point(i, j)

    val edgesMap = for
      Point(x, y) <- nodes
      nextPoints = Point(x + 1, y) :: Point(x, y + 1) :: Point(x - 1, y) :: Point(x, y - 1) :: Nil
      filteredNextPoints = nextPoints
        .filter(p => p.x >= 0 && p.y >= 0 && p.x < matrix.size && p.y < matrix.head.size)
        .filter(p => isValid(Point(x, y), p, matrix))
    yield (Point(x, y) -> filteredNextPoints)

    val edges = edgesMap.flatMap((k, v) => v.map((k, _)))

    Graph(nodes.toSet, edges.toList)

  def isDistanceSmaller(source: Option[Int], target: Option[Int]): Boolean =
    (source, target) match
      case (None, None)       => false
      case (Some(_), None)    => true
      case (None, Some(_))    => false
      case (Some(s), Some(t)) => s + 1 < t

  def BellmanFord(graph: Graph, start: Point) =
    val distance: mutable.Map[Point, Int] = mutable.Map.empty
    val predecessor: mutable.Map[Point, Point] = mutable.Map.empty
    distance(start) = 0

    for (_ <- 0 to graph.edges.size)
      graph.edges.foreach((u, v) =>
        if (isDistanceSmaller(distance.get(u), distance.get(v)))
          distance(v) = distance(u) + 1
          predecessor(v) = u
      )
    (distance, predecessor)

  def parse(lines: List[String]): List[List[Char]] =
    lines.foldLeft(List.empty[List[Char]])((acc, el) => acc.appended(el.toCharArray.toList))

  def getPosition(c: Char, matrix: List[List[Char]]): Point =
    val x = matrix.indexWhere(_.contains(c))
    val y = matrix(x).indexOf(c)
    Point(x, y)

  def main(args: Array[String]): Unit =
    val input = FileReader.readLines(index)
    val parsed = parse(input)
    val graph = convertToGraph(parsed)
    println(graph.nodes.size)
    val start = getPosition('S', parsed)
    val end = getPosition('E', parsed)
    println(start)
    val (distance, predecessor) = BellmanFord(graph, start)


    println(distance(end))
