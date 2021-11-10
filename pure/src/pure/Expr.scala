package pure

sealed trait Expr extends Expr.term {
  def typ: Type
  def inst(su: Map[Param, Type]): Expr
}

object Expr extends Alpha[Expr, Var] {}

class VarList(vars: List[Var]) extends Expr.xs(vars) {
  def inst(su: Map[Param, Type]) = vars map (_ inst su)
}

class ExprList(exprs: List[Expr]) extends Expr.terms(exprs) {
  def typ = exprs map (_.typ)
  def inst(su: Map[Param, Type]) = exprs map (_ inst su)
}

case class Var(name: String, typ: Type, index: Option[Int] = None)
    extends Expr
    with Expr.x {
  def fresh(index: Int): Var =
    Var(name, typ, Some(index))
  def inst(su: Map[Param, Type]) =
    Var(name, typ subst su, index)

  override def toString = index match {
    case None        => name
    case Some(index) => name + "#" + index
  }
}

case class Fun(name: String, params: List[Param], args: List[Type], res: Type) {
  def gen = {
    val re = Type.fresh(params)
    Inst(this, args rename re, res rename re)
  }

  def paramsToString =
    params.mkString("forall ", ", ", ". ")
    
  def argsToString =
    args.mkString(" * ") + " -> "

  override def toString = (params, args) match {
    case (Nil, Nil) =>
      name + ": " + res
    case (Nil, _) =>
      name + ": " + argsToString + res
    case (_, Nil) =>
      name + ": " + paramsToString + res
    case _ =>
      name + ": " + paramsToString + argsToString + res
  }
}

case class Inst(fun: Fun, args: List[Type], res: Type) {
  def subst(su: Map[Param, Type]) =
    Inst(fun, args subst su, res subst su)

  override def toString = fun.name
}

case class App(inst: Inst, args: List[Expr]) extends Expr {
  def fun = inst.fun
  val typ = inst.res
  def free = args.free
  def rename(re: Map[Var, Var]) =
    App(inst, args rename re)
  def subst(su: Map[Var, Expr]) =
    App(inst, args subst su)
  def inst(su: Map[Param, Type]) =
    App(inst subst su, args inst su)

  override def toString =
    if (args.isEmpty)
      inst.toString
    else
      inst + args.mkString("(", ", ", ")")
}

case class Quant(name: String)

case class Bind(quant: Quant, formals: List[Var], body: Expr)
    extends Expr
    with Expr.bind[Bind] {
  def typ = Sort.bool
  def free = body.free -- formals
  def bound = Set(formals: _*)
  def rename(a: Map[Var, Var], re: Map[Var, Var]) =
    Bind(quant, formals rename a, body rename re)
  def subst(a: Map[Var, Var], su: Map[Var, Expr]) =
    Bind(quant, formals rename a, body subst su)
  def inst(su: Map[Param, Type]) =
    Bind(quant, formals inst su, body inst su)
}
