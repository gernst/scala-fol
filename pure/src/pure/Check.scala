package pure

class Check(sig: Sig) {
  var su: Map[Param, Type] = Map()

  case class Pre(private[Check] val expr: Expr) {
    def resolve = expr inst su
  }

  def unify(typ1: Type, typ2: Type) {
    su = Type.unify(typ1, typ2, su)
  }

  def unify(types1: List[Type], types2: List[Type]) {
    su = Type.unify(types1, types2, su)
  }

  def x(name: String, typ: Type) = {
    Pre(Var(name, typ))
  }

  def const(name: String) = {
    app(name, Nil)
  }

  def app(name: String, args: List[Pre]) = {
    val fun = sig.funs(name)
    val inst = fun.gen
    val exprs = args map (_.expr)
    unify(inst.args, exprs.typ)
    Pre(App(inst, exprs))
  }

  def check(arg: Pre, typ: Type) {
    unify(arg.expr.typ, typ)
  }
}
