package fol

sealed trait Expr extends Expr.term with Sugar.expr {
  def typ: Type
}

object Expr extends Alpha[Expr, Var] {
  import Sugar._

  def and(xs: Iterable[Expr]): Expr = {
    if (xs.isEmpty) True
    else xs reduce (_ && _)
  }

  def or(xs: Iterable[Expr]): Expr = {
    if (xs.isEmpty) False
    else xs reduce (_ || _)
  }

  def eqs(xs: Iterable[(Expr, Expr)]): Expr = {
    val zs = for ((x, y) <- xs)
      yield x === y
    and(zs)
  }

  def eqs(xs: Iterable[Expr], ys: Iterable[Expr]): Expr = {
    assert(xs.size == ys.size)
    val zs = for ((x, y) <- (xs zip ys))
      yield x === y
    and(zs)
  }

  object ite extends ternary(Fun.ite)

  object exp extends binary(Fun.exp)
  object times extends binary(Fun.times)
  object divBy extends binary(Fun.divBy)
  object mod extends binary(Fun.mod)

  object uminus extends unary(Fun.uminus)
  object plus extends binary(Fun.plus)
  object minus extends binary(Fun.minus)

  object _eq extends binary(Fun._eq)
  object lt extends binary(Fun.lt)
  object le extends binary(Fun.le)
  object gt extends binary(Fun.gt)
  object ge extends binary(Fun.ge)

  object not extends unary(Fun.not)
  object and extends binary(Fun.and)
  object or extends binary(Fun.or)
  object imp extends binary(Fun.imp)
  object eqv extends binary(Fun.eqv)

  object cons extends binary(Fun.cons)
  object in extends binary(Fun.in)
  object head extends unary(Fun.head)
  object tail extends unary(Fun.tail)
  object last extends unary(Fun.last)
  object init extends unary(Fun.init)

  object select extends binary(Fun.select)
  object store extends ternary(Fun.store)
}

object Const {
  def apply(name: String, typ: Type) = {
    App(Fun(name, Nil, typ), Nil)
  }

  def unapply(expr: Expr) = expr match {
    case App(Fun(name, Nil, typ, _), Nil) =>
      Some((name, typ))
    case _ =>
      None
  }

  def int(n: Int) = Const(n.toString, Sort.int)
  def bool(b: Boolean) = Const(b.toString, Sort.bool)

  def nil(typ: Type.list) = new App(Fun.nil, Nil) {
    assert(fun.args.isEmpty)
    override val env = Type.instantiate(fun.ret, typ, Typing.empty)
  }
}

case class Var(name: String, typ: Type, index: Option[Int] = None) extends Expr with Expr.x {
  def fresh(index: Int) = Var(name, typ, Some(index))
  override def toString = name __ index
}

case class App(fun: Fun, args: List[Expr]) extends Expr {
  val env = Type.instantiate(fun.args, args map (_.typ), Typing.empty)

  def typ = fun.ret subst env
  def free = Set(args flatMap (_.free): _*)
  def rename(re: Ren) = App(fun, args map (_ rename re))
  def subst(su: Subst) = App(fun, args map (_ subst su))

  override def toString = fun.format(args, 0, Non)
}

sealed trait Quant {
  def close(body: Expr, trigger: Set[Expr] = Set()): Expr = {
    val xs = body.free
    if (xs.isEmpty) body
    else Bind(this, xs, body)
  }

  def apply(bound: Iterable[Var], body: Expr): Expr = {
    apply(bound.toSet, body)
  }

  def apply(bound: Set[Var], body: Expr): Expr = {
    val xs = bound & body.free
    if (xs.isEmpty) body
    else body match {
      case Bind(q, ys, body) if q == this =>
        Bind(this, xs ++ ys, body)
      case _ =>
        Bind(this, xs, body)
    }
  }
}

case object All extends Quant {
  override def toString = "forall"
}

case object Ex extends Quant {
  override def toString = "exists"
}

case class Bind(quant: Quant, bound: Set[Var], body: Expr) extends Expr with Expr.bind {
  assert(!bound.isEmpty)
  assert(body.typ == Sort.bool)
  def typ = Sort.bool
  def free = body.free -- bound

  def skolem = {
    val a = Expr.fresh(bound)
    body rename a
  }

  def rename(a: Ren, re: Ren) = {
    Bind(quant, bound map (_ rename a), body rename re)
  }

  def subst(a: Ren, su: Subst) = {
    Bind(quant, bound map (_ rename a), body subst su)
  }

  override def toString = {
    "(" + quant + bound.mkString(" ", ", ", ". ") + body + ")"
  }
}