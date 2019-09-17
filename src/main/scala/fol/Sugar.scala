package fol

object Sugar {
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

  trait expr {
    this: Expr =>
    def ?(left: Expr, right: Expr) = this match {
      case True => left
      case False => right
      case _ => Expr.ite(this, left, right)
    }

    def ^(that: Expr) = Expr.exp(this, that)
    def *(that: Expr) = Expr.times(this, that)
    def /(that: Expr) = Expr.divBy(this, that)
    def %(that: Expr) = Expr.mod(this, that)

    def unary_- = Expr.uminus(this)
    def +(that: Expr) = Expr.plus(this, that)
    def -(that: Expr) = Expr.minus(this, that)

    def ===(that: Expr) = if (this == that) {
      True
    } else if (this.typ == Sort.bool && that.typ == Sort.bool) {
      this <=> that
    } else {
      Expr._eq(this, that)
    }

    def !==(that: Expr) = !(this === that)

    def <=(that: Expr) = Expr.le(this, that)
    def <(that: Expr) = Expr.lt(this, that)
    def >=(that: Expr) = Expr.ge(this, that)
    def >(that: Expr) = Expr.gt(this, that)

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

    def isEmpty = typ match {
      case typ: Type.list => this === Const.nil(typ)
    }

    def ::(that: Expr) = Expr.cons(that, this)
    def in(that: Expr) = Expr.in(this, that)
    def head = Expr.head(this)
    def tail = Expr.tail(this)
    def last = Expr.last(this)
    def init = Expr.init(this)

    def select(index: Expr) = Expr.select(this, index)
    def store(index: Expr, arg: Expr) = Expr.store(this, index, arg)
  }
}