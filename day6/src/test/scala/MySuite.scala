// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day6 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "4166")
    assertEquals(score2, "42250")

  test("Day6 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "17")
    assertEquals(score2, "16")
