package fol

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream

object SMT2 {
  def z3(timeout: Int) = new SMT2("z3", "-t:" + timeout, "-in") {
    override def declare_list() {} // Z3 has builtin lists
  }

  def cvc4(timeout: Int) = new SMT2("cvc4", "--tlimit=" + (timeout * 1000), "--lang=smt2", "--increment-triggers") {
    command("set-logic", "ALL")
  }
}

class SMT2(args: String*) extends Solver {
  case class State(
    var sorts: Set[String],
    var funs: Set[String],
    var vars: Set[String],
    var rcmds: List[String]) {
    def cmds = rcmds.reverse
  }

  val pb = new ProcessBuilder(args: _*)
  val pr = pb.start()
  val stdout = new BufferedReader(new InputStreamReader(pr.getInputStream))
  val stderr = pr.getErrorStream
  val stdin = new PrintStream(pr.getOutputStream)

  object State {
    val empty = State(Set(), Set(), Set(), List())
  }

  var stack = List(State.empty)
  def state = stack.head

  command("set-option", ":print-success", "true")
  command("set-option", ":produce-assertions", "true")
  command("set-logic", "ALL")

  def write(line: String) {
    // println("SMT > " + line)
    stdin.println(line)
    stdin.flush()
  }

  def read(): String = {
    val line = stdout.readLine.trim
    // println("SMT < " + line)
    line
  }

  def push() = {
    stack = state.copy() :: stack
    command("push")
  }

  def pop() = {
    stack = stack.tail
    command("pop")
  }

  override def toString() = {
    state.cmds.mkString("\n")
  }

  def isConsistent: Boolean = {
    write("(check-sat)")
    val out = read()

    out match {
      case "sat" =>
        true
      case "unsat" =>
        false
      case _ =>
        throw ProofUnknown(this)
    }
  }

  def sexpr(arg0: String, args: String*) = {
    "(" + arg0 + " " + args.mkString(" ") + ")"
  }

  def sexpr(args: Iterable[String]) = {
    args mkString ("(", " ", ")")
  }

  def command(name: String, args: String*) = {
    val lengths = args map (_.length)
    val break = name.length + lengths.sum > 80 || !lengths.isEmpty && lengths.max > 20

    val line = new StringBuilder

    line append "("
    line append name
    for (arg <- args) {
      if (break)
        line append "\n  "
      else
        line append " "
      line append arg
    }
    line append ")"

    val cmd = line.toString
    state.rcmds = cmd :: state.rcmds

    write(cmd)

    if (!pr.isAlive)
      throw ProofError(cmd, pr.exitValue)

    val out = read()
    if (out != "success") {
      println(this)
      throw ProofError(cmd, out)
    }
  }

  def declare_sort(sort: Sort) {
    val Sort(name) = sort
    if (!(state.sorts contains name)) {
      state.sorts += name
      command("declare-sort", name, "0")
    }
  }

  def declare_list() {
    val name = "List"
    if (!(state.sorts contains name)) {
      state.sorts += name
      command("declare-datatypes", "((" + name + " 1))", "((par (T) ((nil) (insert (head T) (tail (" + name + " T))))))")
      // command("declare-datatypes", "(T)", "((" + name + " nil (cons (head T) (tail " + name + "))))")
    }
  }

  def declare_fun(fun: Fun) {
    val name = fun.name.toString
    val args = fun.args
    val ret = fun.ret
    if (!(state.funs contains name)) {
      state.funs += name
      command("declare-fun", name, sexpr(args map smt), smt(ret))
    }
  }

  def declare_var(name: String, typ: Type) {
    if (!(state.vars contains name)) {
      state.vars += name
      command("declare-const", name, smt(typ))
    }
  }

  def declare_typ(typ: Type): Unit = typ match {
    case Sort.bool =>
    case Sort.int =>

    case sort: Sort =>
      declare_sort(sort)
    case Type.list(elem) =>
      declare_typ(elem)
      declare_list()
    case Type.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
  }

  def assumeDistinct(exprs: Iterable[Expr]) = {
    val args = exprs map smt
    command("assert", sexpr("distinct", args.toSeq: _*))
  }

  def assume(phi: Expr) {
    command("assert", smt(phi))
  }

  def assert(phi: Expr) {
    assume(!phi)
  }

  def check_sat() {
    command("check-sat")
  }

  def smt(typ: Type): String = typ match {
    case sort @ Sort(name) =>
      declare_typ(typ)
      name
    case Type.list(elem) =>
      declare_list()
      sexpr("List", smt(elem))
    case Type.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
      sexpr("Array", smt(dom), smt(ran))
  }

  def bind(x: Var) = {
    val Var(name, typ, index) = x
    sexpr(smt(x), smt(typ))
  }

  def smt(expr: Expr): String = expr match {
    case Var(name, typ, None) =>
      declare_var(name, typ)
      name
    case Var(name, typ, Some(index)) =>
      declare_var(name + index, typ)
      name + index
    case True =>
      "true"
    case False =>
      "false"
    case Const(name, Sort.int) if name.toString forall (_.isDigit) =>
      name.toString

    case Expr.ite(arg1, arg2, arg3) =>
      sexpr("ite", smt(arg1), smt(arg2), smt(arg3))

    case Expr.times(arg1, arg2) =>
      sexpr("*", smt(arg1), smt(arg2))
    case Expr.mod(arg1, arg2) =>
      sexpr("mod", smt(arg1), smt(arg2))
    case Expr.divBy(arg1, arg2) =>
      sexpr("div", smt(arg1), smt(arg2))
    //    case Expr.exp(arg1, arg2) =>
    //      sexpr("exp", smt(arg1), smt(arg2))

    case Expr.uminus(arg) =>
      sexpr("-", smt(arg))
    case Expr.plus(arg1, arg2) =>
      sexpr("+", smt(arg1), smt(arg2))
    case Expr.minus(arg1, arg2) =>
      sexpr("-", smt(arg1), smt(arg2))

    case Expr._eq(arg1, arg2) =>
      sexpr("=", smt(arg1), smt(arg2))
    case Expr.lt(arg1, arg2) =>
      sexpr("<", smt(arg1), smt(arg2))
    case Expr.le(arg1, arg2) =>
      sexpr("<=", smt(arg1), smt(arg2))
    case Expr.gt(arg1, arg2) =>
      sexpr(">", smt(arg1), smt(arg2))
    case Expr.ge(arg1, arg2) =>
      sexpr(">=", smt(arg1), smt(arg2))

    case Expr.not(arg) =>
      sexpr("not", smt(arg))
    case Expr.and(arg1, arg2) =>
      sexpr("and", smt(arg1), smt(arg2))
    case Expr.or(arg1, arg2) =>
      sexpr("or", smt(arg1), smt(arg2))
    case Expr.imp(arg1, arg2) =>
      sexpr("=>", smt(arg1), smt(arg2))
    case Expr.eqv(arg1, arg2) =>
      sexpr("=", smt(arg1), smt(arg2))

    case App(Fun.nil, List()) =>
      "nil"
    case Expr.cons(arg1, arg2) =>
      sexpr("insert", smt(arg1), smt(arg2))
    case Expr.head(arg) =>
      sexpr("head", smt(arg))
    case Expr.tail(arg) =>
      sexpr("tail", smt(arg))

    case Expr.select(arg1, arg2) =>
      sexpr("select", smt(arg1), smt(arg2))
    case Expr.store(arg1, arg2, arg3) =>
      sexpr("store", smt(arg1), smt(arg2), smt(arg3))

    case App(fun, Nil) =>
      declare_fun(fun)
      fun.toString

    case App(fun, args) =>
      declare_fun(fun)
      sexpr(fun.toString, args map smt: _*)

    case Bind(q, bound, body) =>
      sexpr(q.toString, sexpr(bound map bind), smt(body))
  }
}