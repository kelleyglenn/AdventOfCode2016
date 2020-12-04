package day1

object Pedestrian {
  def distanceToEnd(directions: Seq[(Turn.Value, Int)]): Int = {
    directions.foldLeft(Facing.N, (0, 0)) {
      case ((facing, position), direction) =>
        turnAndStep(facing, position, direction)
    } match {
      case (_, (north, east)) => Math.abs(north) + Math.abs(east)
    }
  }

  def turnAndStep(curFacing: Facing.Value,
                  curPos: (Int, Int),
                  direction: (Turn.Value, Int)): (Facing.Value, (Int, Int)) = {
    val newFacing: Facing.Value = (curFacing, direction._1) match {
      case (Facing.N, Turn.L) => Facing.W
      case (Facing.N, Turn.R) => Facing.E
      case (Facing.S, Turn.L) => Facing.E
      case (Facing.S, Turn.R) => Facing.W
      case (Facing.E, Turn.L) => Facing.N
      case (Facing.E, Turn.R) => Facing.S
      case (Facing.W, Turn.L) => Facing.S
      case (Facing.W, Turn.R) => Facing.N
    }
    val newNorth: Int = newFacing match {
      case Facing.N => curPos._1 + direction._2
      case Facing.S => curPos._1 - direction._2
      case _ => curPos._1
    }
    val newEast: Int = newFacing match {
      case Facing.E => curPos._2 + direction._2
      case Facing.W => curPos._2 - direction._2
      case _ => curPos._2
    }
    (newFacing, (newNorth, newEast))
  }

  def parseDirections(directions: String): Seq[(Turn.Value, Int)] = {
    directions.split(", ").map { (s: String) =>
      (Turn.withName(s.head.toString), s.tail.toInt)
    }
  }
}

object Facing extends Enumeration {
  val N, S, E, W = Value
}
object Turn extends Enumeration {
  val L, R = Value
}
