// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day8 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "41521")
    assertEquals(score2, "19990")

  test("Day8 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "138")
    assertEquals(score2, "66")
