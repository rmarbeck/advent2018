// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day10 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1.split('\n').map(_.trim).mkString,
      """
        |#....#..######...####...#....#..#####...#####...######..#####.
        |#....#..#.......#....#..#....#..#....#..#....#.......#..#....#
        |.#..#...#.......#........#..#...#....#..#....#.......#..#....#
        |.#..#...#.......#........#..#...#....#..#....#......#...#....#
        |..##....#####...#.........##....#####...#####......#....#####.
        |..##....#.......#.........##....#....#..#.........#.....#....#
        |.#..#...#.......#........#..#...#....#..#........#......#....#
        |.#..#...#.......#........#..#...#....#..#.......#.......#....#
        |#....#..#.......#....#..#....#..#....#..#.......#.......#....#
        |#....#..######...####...#....#..#####...#.......######..#####.""".stripMargin.split('\n').map(_.trim).mkString)
    assertEquals(score2, "10124")

  test("Day10 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1.split('\n').map(_.trim).mkString,
          """
            |#...#..###
            |#...#...#.
            |#...#...#.
            |#####...#.
            |#...#...#.
            |#...#...#.
            |#...#...#.
            |#...#..###""".stripMargin.split('\n').map(_.trim).mkString)

    assertEquals(score2, "3")