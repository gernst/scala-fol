package fol

sealed trait Expr extends Expr.term {
  def typ: Type

  def unary_! = this match {
    case True => False
    case False => True
    case Expr.not(phi) => phi
    case _ => Expr.not(this)
  }

  def &&(that: Expr) = (this, that) match {
    case (True, _) => that
    case (False, _) => False
    case (_, True) => this
    case (_, False) => False
    case _ => Expr.and(this, that)
  }

  def ||(that: Expr) = (this, that) match {
    case (True, _) => True
    case (False, _) => that
    case (_, True) => True
    case (_, False) => this
    case _ => Expr.or(this, that)
  }

  def ==>(that: Expr): Expr = (this, that) match {
    case (True, _) => that
    case (False, _) => True
    case (_, True) => True
    case (_, False) => !this
    case _ => Expr.imp(this, that)
  }

  def <=>(that: Expr) = (this, that) match {
    case (True, _) => that
    case (False, _) => !that
    case (_, True) => this
    case (_, False) => !this
    case _ => Expr.eqv(this, that)
  }

  def unary_- = Expr.uminus(this)
  def +(that: Expr) = Expr.plus(this, that)
  def -(that: Expr) = Expr.minus(this, that)
  def *(that: Expr) = Expr.times(this, that)
  def /(that: Expr) = Expr.divBy(this, that)
  def %(that: Expr) = Expr.mod(this, that)
  def ^(that: Expr) = Expr.exp(this, that)

  def <=(that: Expr) = Expr.le(this, that)
  def <(that: Expr) = Expr.lt(this, that)
  def >=(that: Expr) = Expr.ge(this, that)
  def >(that: Expr) = Expr.gt(this, that)

  def ===(that: Expr) = if (this == that) {
    True
  } else if (this.typ == Sort.bool && that.typ == Sort.bool) {
    this <=> that
  } else {
    assert(this.typ == that.typ, "ill-typed: " + this + " == " + that)
    val typed_equals = Pred._eq(typ)
    App(typed_equals, List(this, that))
  }

  def ?(left: Expr, right: Expr) = this match {
    case True => left
    case False => right
    case _ =>
      App(Fun.ite(left.typ), List(this, left, right))
  }

  def !==(that: Expr) = !(this === that)

  def ::(that: Expr) = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    val fun = Fun.cons(typ)
    App(fun, List(that, this))
  }

  def in(that: Expr) = {
    val typ: Type.list = that.typ.asInstanceOf[Type.list]
    val fun = Fun.in(typ)
    App(fun, List(this, that))
  }

  def head = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    val fun = Fun.head(typ)
    App(fun, List(this))
  }

  def last = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    val fun = Fun.last(typ)
    App(fun, List(this))
  }

  def init = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    val fun = Fun.init(typ)
    App(fun, List(this))
  }

  def tail = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    val fun = Fun.tail(typ)
    App(fun, List(this))
  }

  def isEmpty = {
    val typ: Type.list = this.typ.asInstanceOf[Type.list]
    this === Const.nil(typ)
  }

  def select(index: Expr) = {
    val typ: Type.array = this.typ.asInstanceOf[Type.array]
    val fun = Fun.select(typ)
    App(fun, List(this, index))
  }

  def store(index: Expr, arg: Expr) = {
    val typ: Type.array = this.typ.asInstanceOf[Type.array]
    val fun = Fun.store(typ)
    App(fun, List(this, index, arg))
  }
}

object Eq {
  def apply(l: Expr, r: Expr) = {
    App(Pred._eq(l.typ), List(l, r))
  }

  def unapply(e: Expr) = e match {
    case App(fun, List(arg1, arg2)) if fun.name == "==" =>
      Some((arg1, arg2))
    case App(Pred.eqv, List(arg1, arg2)) =>
      Some((arg1, arg2))
    case _ =>
      None
  }
}

object Ite {
  def unapply(e: Expr) = e match {
    case App(fun, List(arg1, arg2, arg3)) if fun.name == Name.ite =>
      Some((arg1, arg2, arg3))
    case _ =>
      None
  }
}

object Expr extends Alpha[Expr, Var] {
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

  class unary(val fun: Fun) {
    def unapply(pure: Expr) = pure match {
      case App(`fun`, List(arg)) => Some(arg)
      case _ => None
    }

    def apply(arg: Expr) = {
      App(fun, List(arg))
    }
  }

  class binary(val fun: Fun) {
    def unapply(pure: Expr) = pure match {
      case App(`fun`, List(arg1, arg2)) => Some((arg1, arg2))
      case _ => None
    }

    def apply(arg1: Expr, arg2: Expr) = {
      App(fun, List(arg1, arg2))
    }

    def flatten(expr: Expr): List[Expr] = expr match {
      case App(`fun`, List(arg1, arg2)) =>
        flatten(arg1) ++ flatten(arg2)
      case _ =>
        List(expr)
    }
  }

  class ternary(val fun: Fun) {
    def unapply(pure: Expr) = pure match {
      case App(`fun`, List(arg1, arg2, arg3)) => Some((arg1, arg2, arg3))
      case _ => None
    }

    def apply(arg1: Expr, arg2: Expr, arg3: Expr): Expr = {
      App(fun, List(arg1, arg2, arg3))
    }
  }

  // object _eq extends binary(Pred.eq)

  object not extends unary(Pred.not)
  object and extends binary(Pred.and)
  object or extends binary(Pred.or)
  object imp extends binary(Pred.imp)
  object eqv extends binary(Pred.eqv)

  object uminus extends unary(Fun.uminus)
  object plus extends binary(Fun.plus)
  object minus extends binary(Fun.minus)
  object times extends binary(Fun.times)
  object divBy extends binary(Fun.divBy)
  object mod extends binary(Fun.mod)
  object exp extends binary(Fun.exp)

  object lt extends binary(Pred.lt)
  object le extends binary(Pred.le)
  object gt extends binary(Pred.gt)
  object ge extends binary(Pred.ge)

  // object question extends ternary("?:")

  // object array_select extends binary("select")
  // object array_store extends ternary("update")
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

  def nil(typ: Type.list) = App(Fun.nil(typ), Nil)
}

case class Var(name: String, typ: Type, index: Option[Int] = None) extends Expr with Expr.x {
  def fresh(index: Int) = Var(name, typ, Some(index))
  override def toString = name.toString
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