// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day4 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "8421")
    assertEquals(score2, "83359")

  test("Day4 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "240")
    assertEquals(score2, "4455")
