package day1

object Pedestrian {
  {
    N.right = E
    N.left = W
    S.right = W
    S.left = E
  }

  def distanceToEnd(directions: Seq[(Turn.Value, Int)]): Int = {
    directions.foldLeft(N.asInstanceOf[Facing], (0, 0)) {
      case ((facing, position), direction) =>
        turnAndStep(facing, position, direction)
    } match {
      case (_, (north, east)) => Math.abs(north) + Math.abs(east)
    }
  }

  def turnAndStep(curFacing: Facing,
                  curPos: (Int, Int),
                  direction: (Turn.Value, Int)): (Facing, (Int, Int)) = {
    val newFacing: Facing = direction._1 match {
      case Turn.L => curFacing.left
      case Turn.R => curFacing.right

    }
    val newNorth: Int = newFacing match {
      case N => curPos._1 + direction._2
      case S => curPos._1 - direction._2
      case _ => curPos._1
    }
    val newEast: Int = newFacing match {
      case E => curPos._2 + direction._2
      case W => curPos._2 - direction._2
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

class Facing(var left: Facing = null, var right: Facing = null)

object N extends Facing

object S extends Facing

object E extends Facing(N, S)

object W extends Facing(S, N)

object Turn extends Enumeration {
  val L, R = Value
}
