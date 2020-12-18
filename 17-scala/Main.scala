import scala.io._

object Part1 {
  type Cell = (Int, Int, Int)

  def nbors(cell: Cell): Seq[Cell] = {
    val (x, y, z) = cell
    for (
      dx <- -1 to 1;
      dy <- -1 to 1;
      dz <- -1 to 1;
      if dx != 0 || dy != 0 || dz != 0
    ) yield (x + dx, y + dy, z + dz)
  }

  def countActiveNbors(cell: Cell, pocket: Set[Cell]): Int =
    nbors(cell)
      .filter(pocket.contains(_))
      .size

  def pocketFromFile(filePath: String): Set[Cell] =
    Source
      .fromFile(filePath)
      .getLines()
      .zipWithIndex
      .flatMap {
        case (line, y) =>
          line
            .zipWithIndex
            .filter(_._1 == '#')
            .map(p => (p._2, y, 0))
            .toList
      }
      .toSet

  def next(pocket: Set[Cell]): Set[Cell] =
    pocket
      .flatMap(nbors)
      .filter {
        case cell => {
          val n = countActiveNbors(cell, pocket)
          if (pocket.contains(cell)) {
            2 <= n && n <= 3
          } else {
            n == 3
          }
        }
      }
}

object Part2 {
  type Cell = (Int, Int, Int, Int)

  def nbors(cell: Cell): Seq[Cell] = {
    val (x, y, z, w) = cell
    for (
      dx <- -1 to 1;
      dy <- -1 to 1;
      dz <- -1 to 1;
      dw <- -1 to 1;
      if dx != 0 || dy != 0 || dz != 0 || dw != 0
    ) yield (x + dx, y + dy, z + dz, w + dw)
  }

  def countActiveNbors(cell: Cell, pocket: Set[Cell]): Int =
    nbors(cell)
      .filter(pocket.contains(_))
      .size

  def pocketFromFile(filePath: String): Set[Cell] =
    Source
      .fromFile(filePath)
      .getLines()
      .zipWithIndex
      .flatMap {
        case (line, y) =>
          line
            .zipWithIndex
            .filter(_._1 == '#')
            .map(p => (p._2, y, 0, 0))
            .toList
      }
      .toSet

  def next(pocket: Set[Cell]): Set[Cell] =
    pocket
      .flatMap(nbors)
      .filter {
        case cell => {
          val n = countActiveNbors(cell, pocket)
          if (pocket.contains(cell)) {
            2 <= n && n <= 3
          } else {
            n == 3
          }
        }
      }
}

object Main {
  def part1(filePath: String): Int =
    LazyList.iterate(Part1.pocketFromFile(filePath))(Part1.next)(6).size

  def part2(filePath: String): Int =
    LazyList.iterate(Part2.pocketFromFile(filePath))(Part2.next)(6).size

  def solveFile(filePath: String) {
    println(s"Input file: $filePath")
    println(s"Part 1: ${part1(filePath)}")
    println(s"Part 2: ${part2(filePath)}")
  }

  def main(args: Array[String]) =
    args.foreach(solveFile)
}
