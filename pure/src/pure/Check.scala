package pure

object Univ {
  def apply(cons: (String, Con)*): Univ = {
    Univ(cons.toMap)
  }

  def param(name: String): Param = {
    Param(name)
  }

  def con(name: String, arity: Int): Con = {
    Con(name, arity)
  }
}

case class Univ(cons: Map[String, Con]) {
  def sort(name: String, args: List[Type]): Sort = {
    val con = cons(name)
    if (con.arity != args.length) fail
    Sort(con, args)
  }
}

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
  ): Typed = {
    val x = vars(name)
    Typed(x, x.typ, su0)
  }

  def app(name: String, exprs: List[Expr])(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): Typed = {
    val fun = funs(name)
    val inst = fun.gen
    val su1 = Typed.unify(inst.args, exprs.typ, su0)
    val expr = App(inst, exprs)
    Typed(expr, inst.res, su1)
  }

  def quant(name: String): Quant = {
    Quant(name)
  }

  def bind(
      quant: Quant,
      formals: List[Var],
      body: (Map[String, Var], Map[Param, Type]) => Typed
  )(
      vars: Map[String, Var],
      su0: Map[Param, Type]
  ): Typed = {
    val vars_ = Map(formals map (x => (x.name, x)): _*)
    val typed = body(vars ++ vars_, su0)
    val Typed(body_, typ, su1) = typed check Sort.bool
    val expr = Bind(quant, formals, body_)
    Typed(expr, typ, su1)
  }
}

// extra class is annoying, because does not work nicely for lists
case class Typed(expr_ : Expr, typ: Type, su: Map[Param, Type]) {
  def expr = {
    val expr__ = expr_ inst su
    val typ__ = typ subst su
    assert(typ__ == expr__.typ)
    expr__
  }

  def check(expected: Type) = {
    val su_ = Typed.unify(typ, expected, su)
    Typed(expr_, typ, su_)
  }
}

object Typed {
  def unify(typ1: Type, typ2: Type, su: Map[Param, Type]): Map[Param, Type] = {
    (typ1, typ2) match {
      case _ if typ1 == typ2 =>
        su
      case (p1: Param, _) if su contains p1 =>
        unify(su(p1), typ2, su)
      case (_, p2: Param) if su contains p2 =>
        unify(typ1, su(p2), su)
      case (p1: Param, _) =>
        if (p1 in typ2) {
          println("recursive unification " + p1 + " in " + typ2)
          fail
        }
        su + (p1 -> typ2)
      case (_, p2: Param) =>
        unify(p2, typ1, su)
      case (Sort(con1, args1), Sort(con2, args2)) if con1 == con2 =>
        unify(args1, args2, su)
      case _ =>
        println("cannot unify " + typ1 + " and " + typ2)
        fail
    }
  }

  def unify(
      types1: List[Type],
      types2: List[Type],
      su: Map[Param, Type]
  ): Map[Param, Type] = {
    (types1, types2) match {
      case (Nil, Nil) =>
        su
      case (typ1 :: types1, typ2 :: types2) =>
        unify(types1, types2, unify(typ1, typ2, su))
      case _ =>
        println("cannot unify " + types1 + " and " + types2)
        fail
    }
  }
}
