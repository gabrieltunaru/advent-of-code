import org.scalatest.flatspec.AnyFlatSpec
import day9.Point

class Day9Test extends AnyFlatSpec:
  "A tail" should "move to the head" in {
    val head = Point(2,2)
    val tail = Point(0,0)
    val res = Day9_2.moveTail(head, tail)
    assert(res == Point(1,1))
  }
