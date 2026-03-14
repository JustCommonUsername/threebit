import munit.FunSuite
import threebit.{Config, Interpreter, Prog, Parser}

def testProg(x: Int, y: Int, z: Int, prg: String, expected: String): Unit =
  val config: Config = (0, Map("X" -> x, "Y" -> y, "Z" -> z))
  val prog: Prog = Parser.parseProg(prg)
  val (_, out) = Interpreter.run(prog, config)
  assert(out.map(_.toString).mkString(",") == expected)

class ThreeBitUnitTest extends FunSuite {
  test("firstTest") {
    testProg(3729, 0, 0, "0,1,5,4,3,0", "0,4,2,1,4,2,5,6,7,3,1,0")
  }
  test("secondTest") {
    testProg(8642024, 0, 0, "0,3,5,4,3,0", "5,7,6,5,7,0,4,0")
  }
}
