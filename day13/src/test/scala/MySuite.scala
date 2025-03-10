// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day13 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "100,21")
    assertEquals(score2, "113,109")

  test("Day13 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "2,0")
    assertEquals(score2, "6,5")
