import mclApp.west

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.io.StdIn

object mclApp {
  var sig = 2
  var numParts = 100
  var width: Int = 0
  var height: Int = 0
  var walls: BitSet = BitSet.empty
  var parts: List[(Int, Int, Double)] = List.empty
  val rand = new scala.util.Random()

  @tailrec
  def nextLine: String = {
    val line = StdIn.readLine()
    if (line == null || line == "" || line.charAt(0) == '/') nextLine
    else line
  }

  def xyTOi(x: Int, y: Int): Int = y*width + x
  def iTOxy(i: Int): (Int, Int) = ({i % width}, {i / width})

  def canMove(x: Int, y: Int): Boolean = {
    x >= 0 && x < width &&
    y >= 0 && y < height &&
    !walls(xyTOi(x, y))
  }

  def north(x: Int, y: Int): (Int, Int) = if (canMove(x, y+1)) (x, y+1) else (x, y)
  def south(x: Int, y: Int): (Int, Int) = if (canMove(x, y-1)) (x, y-1) else (x, y)
  def east(x: Int, y: Int): (Int, Int) = if (canMove(x+1, y)) (x+1, y) else (x, y)
  def west(x: Int, y: Int): (Int, Int) = if (canMove(x-1, y)) (x-1, y) else (x, y)

  def moveForward(dir: String, x: Int, y: Int): (Int, Int) = dir match {
    case "NORTH" => north(x, y)
    case "SOUTH" => south(x, y)
    case "EAST" => east(x, y)
    case "WEST" => west(x, y)
//    case e => throw new Exception(s"You can't go in the direction: \"$e\" ")
  }

  def moveRight(dir: String, x: Int, y: Int): (Int, Int) = dir match {
    case "NORTH" => east(x, y)
    case "SOUTH" => west(x, y)
    case "EAST" => south(x, y)
    case "WEST" => north(x, y)
//    case e => throw new Exception(s"You can't go in the direction: \"$e\" ")
  }

  def moveLeft(dir: String, x: Int, y: Int): (Int, Int) = dir match {
    case "NORTH" => west(x, y)
    case "SOUTH" => east(x, y)
    case "EAST" => north(x, y)
    case "WEST" => south(x, y)
//    case e => throw new Exception(s"You can't go in the direction: \"$e\" ")
  }

  def resample(parts: List[(Int, Int, Double)]): List[(Int, Int, Double)] = {
    val sortParts = parts

    var lastPart = 0.0
    var total = 0.0
    val intervals = sortParts.map(part => {
      val dif = part._3/numParts - lastPart
      total += dif
      total
    })

//    intervals.foreach(t => println(t/total))

    var newParts: List[(Int, Int, Double)] = List.empty
    for (_ <- 0 to numParts - numParts/10) {
      val roll = rand.nextDouble()
      val index =
        intervals.zipWithIndex.find(roll <= _._1 / total).getOrElse(
          throw new Exception("roll out of bounds")
        )._2
      newParts = parts(index) +: newParts
    }
    for (_ <- 0 to numParts/10) {
      newParts = (rand.nextInt(width), rand.nextInt(height), 0.0) +: newParts
    }
    newParts
  }

  def main(args: Array[String]): Unit = {
    width = nextLine.toInt
    height = nextLine.toInt

//    if (args.length >= 1) numParts = args(0).toInt
//    if (args.length >= 2) sig = args(1).toInt

    for (y <- height-1 to 0 by -1) {
      val line = nextLine
      for (x <- 0 until width) {
        if (line.charAt(x) == '#') walls += xyTOi(x, y)
      }
    }

    for (_ <- 1 to numParts) {
      parts = (rand.nextInt(width), rand.nextInt(height), 0.0) +: parts
    }
    var line = nextLine
    while (line != "END") {
      val split = line.split(" ")
      val sense = (split(0).toDouble, split(1).toDouble)
      val dir = split(2)

      parts = parts.map(part => {
        val roll = rand.nextInt(11)
        val pos = if (roll <= 7) moveForward(dir, part._1, part._2)
        else if (roll == 8) moveRight(dir, part._1, part._2)
        else if (roll == 9) moveLeft(dir, part._1, part._2)
        else if (roll == 10) (part._1, part._2)
        else throw new Exception(
          "What are you doing here? That die must be broken...")

        val denom = (2*sig)*(2*sig)
        val w = Math.exp(0 - (((pos._2 - sense._2)*(pos._2 - sense._2))/denom +
                              ((pos._1 - sense._1)*(pos._1 - sense._1))/denom))

        (pos._1, pos._2, w)
      })

      parts = resample(parts)

      parts.foreach(part => println(s"${part._1} ${part._2} ${part._3}"))

      line = nextLine
    }
  }
}
