// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day9 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "398730")
    assertEquals(score2, "3349635509")

  test("Day9 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "8317")
    assertEquals(score2, "74868857")
