package fol

import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStream
import scala.io.Source

object SMT2 {
  def z3(timeout: Int) = new SMT2("z3", "-T:" + timeout, "-in") {
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

  object State {
    val empty = State(Set(), Set(), Set(), List())
  }

  var stack = List(State.empty)
  def state = stack.head

  def push() = {
    stack = state.copy() :: stack
  }

  def pop() = {
    stack = stack.tail
  }

  override def toString() = {
    state.cmds.mkString("\n")
  }

  def isConsistent: Boolean = {
    val pb = new ProcessBuilder(args: _*)
    val pr = pb.start()
    val stdout = pr.getInputStream
    val stderr = pr.getErrorStream
    val stdin = new PrintStream(pr.getOutputStream)

    for (cmd <- state.cmds) {
      stdin.append(cmd)
    }

    stdin.append("(check-sat)")
    stdin.flush()
    stdin.close() // EOF

    val out = Source.fromInputStream(stdout).mkString
    val err = Source.fromInputStream(stdout).mkString

    pr.waitFor()
    val exit = pr.exitValue
    if (exit != 0) {
      throw ProofError(exit, out, err)
    }

    if (out == null) {
      throw ProofError(err)
    } else {
      out.trim match {
        case "sat" =>
          true
        case "unsat" =>
          false
        case _ =>
          throw ProofUnknown(this)
      }
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

    cmd
  }

  def declare_sort(sort: Sort) {
    val Sort(name) = sort
    if (!(state.sorts contains name)) {
      state.sorts += name
      command("declare-sort", name, "0")
    }
  }

  def declare_pointer() {
    val name = "Pointer"
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
    case Sort.list(elem) =>
      declare_typ(elem)
      declare_list()
    case Sort.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
    case Sort.pointer(elem) =>
      declare_typ(elem)
      declare_pointer()
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
    case Sort.list(elem) =>
      declare_list()
      sexpr("List", smt(elem))
    case Sort.array(dom, ran) =>
      declare_typ(dom)
      declare_typ(ran)
      sexpr("Array", smt(dom), smt(ran))
    case Sort.pointer(elem) =>
      declare_typ(typ)
      "Pointer"
  }

  def bind(x: Var) = {
    val Var(name, typ) = x
    sexpr(smt(x), smt(typ))
  }

  def smt(expr: Expr): String = expr match {
    case Var(Name(name, None), typ) =>
      declare_var(name, typ)
      name
    case Var(Name(name, Some(index)), typ) =>
      declare_var(name + index, typ)
      name + index
    case True =>
      "true"
    case False =>
      "false"
    case Const(name, Sort.int) if name.toString forall (_.isDigit) =>
      name.toString

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

    case Expr.uminus(arg) =>
      sexpr("-", smt(arg))
    case Expr.plus(arg1, arg2) =>
      sexpr("+", smt(arg1), smt(arg2))
    case Expr.minus(arg1, arg2) =>
      sexpr("-", smt(arg1), smt(arg2))

    case Expr.times(arg1, arg2) =>
      sexpr("*", smt(arg1), smt(arg2))
    case Expr.mod(arg1, arg2) =>
      sexpr("mod", smt(arg1), smt(arg2))
    case Expr.divBy(arg1, arg2) =>
      sexpr("div", smt(arg1), smt(arg2))
    //    case Expr.exp(arg1, arg2) =>
    //      sexpr("exp", smt(arg1), smt(arg2))

    case Expr.lt(arg1, arg2) =>
      sexpr("<", smt(arg1), smt(arg2))
    case Expr.le(arg1, arg2) =>
      sexpr("<=", smt(arg1), smt(arg2))
    case Expr.gt(arg1, arg2) =>
      sexpr(">", smt(arg1), smt(arg2))
    case Expr.ge(arg1, arg2) =>
      sexpr(">=", smt(arg1), smt(arg2))

    case Eq(arg1, arg2) =>
      sexpr("=", smt(arg1), smt(arg2))
    case Ite(arg1, arg2, arg3) =>
      sexpr("ite", smt(arg1), smt(arg2), smt(arg3))

    case App(Fun.total(Name.select, _, _, _), List(arg1, arg2)) =>
      sexpr("select", smt(arg1), smt(arg2))
    case App(Fun.total(Name.store, _, _, _), List(arg1, arg2, arg3)) =>
      sexpr("store", smt(arg1), smt(arg2), smt(arg3))

    case App(Fun.total(Name.nil, _, typ, _), Nil) =>
      "nil"
    case App(Fun.total(Name.cons, _, typ, _), List(arg1, arg2)) =>
      sexpr("insert", smt(arg1), smt(arg2))
    case App(Fun.partial(Name.head, List(Var(_, typ)), _, _, _), List(arg)) =>
      sexpr("head", smt(arg))
    case App(Fun.partial(Name.tail, List(Var(_, typ)), _, _, _), List(arg)) =>
      sexpr("tail", smt(arg))

    case App(fun, Nil) =>
      declare_fun(fun)
      fun.toString

    case App(fun, args) =>
      declare_fun(fun)
      sexpr(fun.toString, args map smt: _*)

    case Bind(q, bound, body, trigger) =>
      sexpr(q.toString, sexpr(bound map bind), smt(body))
  }
}