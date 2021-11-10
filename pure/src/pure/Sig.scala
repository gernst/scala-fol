package pure

object Sig {
  def apply(funs: (String, Fun)*): Sig = {
    Sig(funs.toMap)
  }

  def fun(
      name: String,
      params: List[Param],
      args: List[Type],
      res: Type
  ): Fun = {
    Fun(name, params, args, res)
  }
}

case class Sig(funs: Map[String, Fun]) {
  def formal(name: String, typ: Type): Var = {
    Var(name, typ)
  }

  def x(name: String)(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): (Var, Map[Param, Type]) = {
    val x = vars(name)
    (x, su0)
  }

  def const(name: String)(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): (App, Map[Param, Type]) = {
    app(name, Nil)(vars, su0)
  }

  def app(name: String, exprs: List[Expr])(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): (App, Map[Param, Type]) = {
    val fun = funs(name)
    val inst = fun.gen
    val su1 = Type.unify(inst.args, exprs.typ, su0)
    val expr = App(inst, exprs)
    (expr, su1)
  }

  def quant(name: String): Quant = {
    Quant(name)
  }

  def bind(
      quant: Quant,
      formals: List[Var],
      inner: (Map[String, Var], Map[Param, Type]) => (
          Expr,
          Map[Param, Type]
      )
  )(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): (Bind, Map[Param, Type]) = {
    val vars_ = Map(formals map (x => (x.name, x)): _*)
    val (body, su1) = inner(vars ++ vars_, su0)
    val su2 = Type.unify(body.typ, Sort.bool, su1)
    val expr = Bind(quant, formals, body)
    (expr, su1)
  }
}
