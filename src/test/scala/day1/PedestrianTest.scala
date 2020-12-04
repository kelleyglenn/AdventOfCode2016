package day1

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class PedestrianTest extends AnyFlatSpec {
  behavior of "parseDirections"
  it should "handle a simple example" in {
    assert(Pedestrian.parseDirections("R2") == List((Turn.R, 2)))
  }
  it should "handle the first example" in {
    assert(
      Pedestrian.parseDirections("R2, L3") == List((Turn.R, 2), (Turn.L, 3)))
  }
  behavior of "distanceToEnd"
  it should "handle the examples" in {
    assert(Pedestrian.distanceToEnd(Pedestrian.parseDirections("R2, L3")) == 5)
    assert(Pedestrian.distanceToEnd(Pedestrian.parseDirections("R2, R2, R2")) == 2)
    assert(Pedestrian.distanceToEnd(Pedestrian.parseDirections("R5, L5, R5, R3")) == 12)
  }
  it should "solve the puzzle" in new SetupPuzzleData {
    assert(Pedestrian.distanceToEnd(Pedestrian.parseDirections(lines.head)) == 298)
  }
}
