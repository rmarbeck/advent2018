// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "3241")
    assertEquals(score2, "2749999999911")

  test("Day12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "325")
    assertEquals(score2, "999999999374")
