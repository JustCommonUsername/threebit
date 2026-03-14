package threebit
import threebit.Combo.{Val, X, Y, Z}

import scala.io.StdIn.readLine

enum Combo:
  case Val(v: Int)
  case X
  case Y
  case Z

enum Operand:
  case ComboOp(c: Combo)
  case LiteralOp(l: Int)
  case JumpOp(l: Int)

import Operand.*

enum Inst:
  case Xdv(op: ComboOp)
  case Yxl(op: LiteralOp)
  case Yst(op: ComboOp)
  case Jnz(op: JumpOp)
  case Yxz
  case Out(op: ComboOp)
  case Ydv(op: ComboOp)
  case Zdv(op: ComboOp)

import Inst.*

type Prog = List[Inst]
type Pc = Int
type Reg = Map[String, Int]
type Out = List[Int]
type Config = (Pc, Reg)

object Interpreter {
  private def eval(reg: Reg, op: Operand): Int = op match {
    case Operand.ComboOp(c) => c match {
      case Combo.Val(v) => v
      case Combo.X => reg("X")
      case Combo.Y => reg("Y")
      case Combo.Z => reg("Z")
    }
    case Operand.LiteralOp(l) => l
    // Divide by two, since we have twice less indices (operators and operands are joined together)
    case Operand.JumpOp(l) => l / 2
  }

  // Maybe make monadic?
  enum State:
    case Running(cfg: Config)
    case Term(cfg: Config)
    case Fault(cfg: Config)

  import State.*
  import scala.math._

  private def isTerm(prog: Prog, pc: Pc) = pc >= prog.length || pc < 0

  private def step(prog: Prog, state: State): (State, Out) = state match {
    case State.Running((pc, reg)) =>
      if (isTerm(prog, pc)) return (Term(pc, reg), List.empty)
      try {
        prog(pc) match {
          case Inst.Xdv(op) =>
            val v = eval(reg, op)
            val x = reg("X")
            val reg_ = reg + ("X" -> (x / pow(2, v)).toInt)
            (Running(pc + 1, reg_), List.empty)
          case Inst.Yxl(op) =>
            val v = eval(reg, op)
            val y = reg("Y")
            val reg_ = reg + ("Y" -> (y ^ v))
            (Running(pc + 1, reg_), List.empty)
          case Inst.Yst(op) =>
            val v = eval(reg, op)
            val reg_ = reg + ("Y" -> v % 8)
            (Running(pc + 1, reg_), List.empty)
          case Inst.Jnz(op) =>
            val v = eval(reg, op)
            val x = reg("X")
            if (x == 0)
              (Running(pc + 1, reg), List.empty)
            else
              (Running(v, reg), List.empty)
          case Inst.Yxz =>
            val y = reg("Y")
            val z = reg("Z")
            (Running(pc + 1, reg + ("Y" -> (y ^ z))), List.empty)
          case Inst.Out(op) =>
            val v = eval(reg, op)
            (Running(pc + 1, reg), List(v % 8))
          case Inst.Ydv(op) =>
            val v = eval(reg, op)
            val x = reg("X")
            val reg_ = reg + ("Y" -> (x / pow(2, v)).toInt)
            (Running(pc + 1, reg_), List.empty)
          case Inst.Zdv(op) =>
            val v = eval(reg, op)
            val x = reg("X")
            val reg_ = reg + ("Z" -> (x / pow(2, v)).toInt)
            (Running(pc + 1, reg_), List.empty)
        }
      } catch {
        case _: Throwable => (Fault(pc, reg), List.empty)
      }
    case _ => (state, List.empty)
  }

  private def steps(prog: Prog, state: State): (State, Out) =
    val (state1, out1) = step(prog, state)
    state1 match {
      case State.Running(cfg) =>
        val (state2, out2) = steps(prog, state1)
        (state2, out1 ++ out2)
      case _ => (state1, out1)
    }

  def run(prog: Prog, cfg: Config): (State, Out) =
    steps(prog, Running(cfg))
}

object Parser {

  private val initialPc: Pc = 0

  private def parseComboOp(op: Int): ComboOp = op match
    case 0 => ComboOp(Val(0))
    case 1 => ComboOp(Val(1))
    case 2 => ComboOp(Val(2))
    case 3 => ComboOp(Val(3))
    case 4 => ComboOp(X)
    case 5 => ComboOp(Y)
    case 6 => ComboOp(Z)
    case _ => throw NotImplementedError("Cannot parse a combo operand with value except for 0..6")

  private def parseInst(inst: Int, op: Int): Inst = inst match
    case 0 => Xdv(parseComboOp(op))
    case 1 => Yxl(LiteralOp(op))
    case 2 => Yst(parseComboOp(op))
    case 3 => Jnz(JumpOp(op))
    case 4 => Yxz
    case 5 => Out(parseComboOp(op))
    case 6 => Ydv(parseComboOp(op))
    case 7 => Zdv(parseComboOp(op))

  def parseProg(str: String): Prog =
    def parseInstrOps(instOps: Seq[(Int, Int)]): Prog =
      instOps.foldRight(List.empty)((instOp, acc) => parseInst(instOp._1, instOp._2) :: acc)
    val tokens = str.split(",").map(_.trim.toInt)
    // Instructions are always on even position in the prog
    // Operands are always on odd position in the prog
    // Count from 0
    val (instrs, ops) = tokens.zipWithIndex.partition((_, v) => v % 2 == 0)
    val instrOps = instrs.zip(ops).map((inst, op) => (inst._1, op._1))
    parseInstrOps(instrOps.toIndexedSeq)

  private def parseReg(str: String): Int = str.toInt

  def parse(): (Prog, Config) =
    val x = parseReg(readLine())
    val y = parseReg(readLine())
    val z = parseReg(readLine())
    val initialReg: Reg = Map("X" -> x, "Y" -> y, "Z" -> z)
    val initialConfig: Config = (initialPc, initialReg)
    val prog = parseProg(readLine())
    (prog, initialConfig)
}

/** Command-line entry for the three bit language */
object ThreeBit extends App {
  import Parser._

  private val (prog, conf) = parse()
  val (state, out) = Interpreter.run(prog, conf)
  println(out.map(_.toString).mkString(","))
  // debug: println(state)
}
