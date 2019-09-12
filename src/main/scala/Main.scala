import scala.util.Try

object Main extends App {
  val dungeon = Dungeon.fromLines(Vector(
    "xxxxxx",
    "x    x",
    "x  xxx",
    "x    x",
    "xxxxxx"
  ))
  dungeon.set(1, 1, Wall)
  println(dungeon)
}

case class Dungeon(data: Array[Array[Cell]]) {
  override def toString: String = {
    data.map(_.map(_.toString).mkString("")).mkString("\n")
  }

  def get(x: Int, y: Int): Option[Cell] = Try {
    data(x)(y)
  }.toOption

  def set(x: Int, y: Int, value: Cell): Unit = data(x)(y) = value
}

object Dungeon {
  def fromLines(text: Vector[String]): Dungeon = {
    Dungeon.fromData(text.map(decodeLine))
  }

  def fromData(data: Vector[Vector[Cell]]): Dungeon = {
    Dungeon(data.map(_.toArray).toArray)
  }

  private def decodeLine(line: String): Vector[Cell] = {
    line.toVector.map(Cell(_))
  }
}

sealed trait Cell

case object Empty extends Cell {
  override def toString: String = " "
}

case object Wall extends Cell {
  override def toString: String = "#"
}

object Cell {
  def apply(c: Char): Cell = {
    c match {
      case ' ' => Empty
      case _ => Wall
    }
  }
}