package check

object Check {
  // def build(
  //     exprs: List[raw.Expr],
  //     vars: Map[String, pure.Var],
  //     su0: Map[pure.Param, pure.Type]
  // ): (List[pure.Expr], List[pure.Type], Map[pure.Param, pure.Type]) = {
  //   exprs match {
  //     case Nil =>
  //       (Nil, Nil, su0)
  //     case expr :: exprs =>
  //       val (pure, typ, su1) = infer(expr, vars, su0)
  //       val (pures, types, su2) = infer(exprs, vars, su1)
  //       (pure :: pures, typ :: types, su2)
  //   }
  // }

  // def build(typ: raw.Type): pure.Type = {
  //   typ match {
  //     case raw.Param(name) =>
  //       pure.Param(name)
  //     case raw.Sort(name, args) =>
  //       val con = cons(name)
  //       assert(con.arity != args.length)
  //       pure.Sort(con, args map build)
  //   }
  // }

  // def build(formal: raw.Formal): pure.Var = {
  //   pure.Var(formal.name, build(formal.typ))
  // }
}
