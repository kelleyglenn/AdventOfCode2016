package day1

object Pedestrian {
  def walk(directions: Seq[(Turn.Value, Int)]): Int = {
    var north, east = 0

    var facing: Facing.Value = Facing.N
    directions.foreach { (direction: (Turn.Value, Int)) =>
      facing = (facing, direction._1) match {
        case (Facing.N, Turn.L) => Facing.W
        case (Facing.N, Turn.R) => Facing.E
        case (Facing.S, Turn.L) => Facing.E
        case (Facing.S, Turn.R) => Facing.W
        case (Facing.E, Turn.L) => Facing.N
        case (Facing.E, Turn.R) => Facing.S
        case (Facing.W, Turn.L) => Facing.S
        case (Facing.W, Turn.R) => Facing.N
      }
      north = facing match {
        case Facing.N => north + direction._2
        case Facing.S => north - direction._2
        case _        => north
      }
      east = facing match {
        case Facing.E => east + direction._2
        case Facing.W => east - direction._2
        case _        => east
      }
    }
    Math.abs(north) + Math.abs(east)
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
