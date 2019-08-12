package fol

sealed trait Fun {
  def name: Name //this is now a function, if we declare it with val will it remain a method or become an abstract field?
  def args: List[Type]
  def ret: Type
  def fixity: Fixity
  def fresh: Fun

  def apply(args: Expr*) = App(this, args.toList)
}

/**
 * Function
 */
object Fun extends Fresh {
  val n = Var("n", Sort.int)
  val d = Var("d", Sort.int)

  /**
   * total
   */
  def apply(name: Name, args: List[Type], ret: Type, fixity: Fixity = Nilfix): Fun = {
    total(name, args, ret, fixity)
  }

  case class total(name: Name, args: List[Type], ret: Type, fixity: Fixity) extends Fun {
    // def apply(args: Expr*) = App(this, args.toList)
    def fresh = total(Var.fresh(name), args, ret, fixity)
    override def toString = name.toString
  }

  case class partial(name: Name, vars: List[Var], ret: Type, dom: Expr, fixity: Fixity) extends Fun {
    assert(dom.free subsetOf vars.toSet)
    def args = vars map (_.typ)
    def fresh = partial(Var.fresh(name), vars, ret, dom, fixity)
    override def toString = name.toString
  }

  //arithmetic functions

  val uminus = Fun("-", List(Sort.int), Sort.int, Prefix(8))
  val plus = Fun("+", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))
  val minus = Fun("-", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))
  val times = Fun("*", List(Sort.int, Sort.int), Sort.int, Infix(Left, 8))
  val exp = Fun("^", List(Sort.int, Sort.int), Sort.int, Infix(Left, 9))

  lazy val divBy = Fun.partial("/", List(n, d), Sort.int, d !== 0, Infix(Non, 8))
  lazy val mod = Fun.partial("%", List(n, d), Sort.int, d !== 0, Infix(Non, 8))

  //list functions

  def nil(list: Sort.list) = {
    Fun(Name.nil, List(), list)
  }

  def _null(typ: Sort.pointer) = {
    Fun(Name._null, List(), typ)
  }

  def cons(list: Sort.list) = {
    val Sort.list(elem) = list
    Fun(Name.cons, List(elem, list), list)
  }

  def in(list: Sort.list) = {
    val Sort.list(elem) = list
    Fun(Name.in, List(elem, list), Sort.bool)
  }

  def head(list: Sort.list) = {
    val x = Var("x", list)
    val Sort.list(elem) = list
    Fun.partial(Name.head, List(x), elem, !x.isEmpty, Nilfix)
  }

  def last(list: Sort.list) = {
    val x = Var("x", list)
    val Sort.list(elem) = list
    Fun.partial(Name.last, List(x), elem, !x.isEmpty, Nilfix)
  }

  def init(list: Sort.list) = {
    val x = Var("x", list)
    val Sort.list(elem) = list
    Fun.partial(Name.init, List(x), list, !x.isEmpty, Nilfix)
  }

  def tail(list: Sort.list) = {
    val x = Var("x", list)
    val Sort.list(elem) = list
    Fun.partial(Name.tail, List(x), list, !x.isEmpty, Nilfix)
  }

  //Array type-functions

  /**
   * Function to generate a select function for any array type
   * @param arr The Type of array for which to generate a select function
   * @return a select fuction arr -> dom -> ran
   */
  def select(arr: Sort.array) = {
    val Sort.array(dom, ran) = arr
    Fun(Name.select, List(arr, dom), ran, Formfix)
  }

  /**
   * Function to generate a store function for any array type
   * @param arr The Type of array for which to generate a store function
   * @return a store fuction arr -> dom -> ran -> arr
   */
  def store(arr: Sort.array) = {
    val Sort.array(dom, ran) = arr
    Fun(Name.store, List(arr, dom, ran), arr, Formfix)
  }

  def ite(typ: Type) = {
    Fun(Name.ite, List(Sort.bool, typ, typ), typ, Formfix)
  }
}

/**
 * Predicate: total, boolean valued function
 */
object Pred {
  def apply(name: Name, args: List[Type], fixity: Fixity = Nilfix): Fun = {
    Fun(name, args, Sort.bool, fixity)
  }

  def unapply(fun: Fun) = fun match {
    case Fun.total(name, args, Sort.bool, _) =>
      Some((name, args))
    case _ =>
      None
  }

  val not = Pred("!", List(Sort.bool), Prefix(5))
  val and = Pred("&&", List(Sort.bool, Sort.bool), Infix(Left, 4))
  val or = Pred("||", List(Sort.bool, Sort.bool), Infix(Left, 3))
  /** implies */
  val imp = Pred("==>", List(Sort.bool, Sort.bool), Infix(Right, 2))
  /** equivalent*/
  val eqv = Pred("<=>", List(Sort.bool, Sort.bool), Infix(Non, 1))

  val le = Pred("<=", List(Sort.int, Sort.int), Infix(Non, 6))
  val lt = Pred("<", List(Sort.int, Sort.int), Infix(Non, 6))
  val ge = Pred(">=", List(Sort.int, Sort.int), Infix(Non, 6))
  val gt = Pred(">", List(Sort.int, Sort.int), Infix(Non, 6))

  /**
   * return an equality predicate for any type
   */
  def _eq(typ: Type) = {
    Pred(Name._eq, List(typ, typ), Infix(Non, 6))
  }
}

sealed trait Expr {
  def typ: Type
  def free: Set[Var]

  // used in programs, ensures e.g. that lhs of choose/assign remains Var
  def rename(re: Ren): Expr
  def subst(su: Subst): Expr
  def eval(st: Subst): Expr
  def eval(sts: List[Subst]): Expr

  def delta: Expr

  //Simplifying Logical operators

  /**
   * ¬ operator that does simplification.
   * Use this instead of Expr.not() directly
   */
  def unary_! = this match {
    case True => False
    case False => True
    case Expr.not(phi) => phi
    case _ => Expr.not(this)
  }

  /**
   * ∧ operator that does simplification.
   * Use this instead of Expr.and() directly
   */
  def &&(that: Expr) = (this, that) match {
    case (True, _) => that
    case (False, _) => False
    case (_, True) => this
    case (_, False) => False
    case _ => Expr.and(this, that)
  }

  /**
   * ∨ operator that does simplification.
   * Use this instead of Expr.or() directly
   */
  def ||(that: Expr) = (this, that) match {
    case (True, _) => True
    case (False, _) => that
    case (_, True) => True
    case (_, False) => this
    case _ => Expr.or(this, that)
  }

  /**
   * ⇒ operator that does simplification.
   * Use this instead of Expr.imp() directly
   */
  def ==>(that: Expr): Expr = (this, that) match {
    case (True, _) => that
    case (False, _) => True
    case (_, True) => True
    case (_, False) => !this
    case _ => Expr.imp(this, that)
  }

  /**
   * => operator that does simplification.
   * Use this instead of Expr.eqv() directly
   */
  def <=>(that: Expr) = (this, that) match {
    case (True, _) => that
    case (False, _) => !that
    case (_, True) => this
    case (_, False) => !this
    case _ => Expr.eqv(this, that)
  }

  //Arithmetic operators

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

  //Equality operators

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

  //List operators

  def ::(that: Expr) = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    val fun = Fun.cons(typ)
    App(fun, List(that, this))
  }

  def in(that: Expr) = {
    val typ: Sort.list = that.typ.asInstanceOf[Sort.list]
    val fun = Fun.in(typ)
    App(fun, List(this, that))
  }

  def head = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    val fun = Fun.head(typ)
    App(fun, List(this))
  }

  def last = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    val fun = Fun.last(typ)
    App(fun, List(this))
  }

  def init = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    val fun = Fun.init(typ)
    App(fun, List(this))
  }

  def tail = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    val fun = Fun.tail(typ)
    App(fun, List(this))
  }

  def isEmpty = {
    val typ: Sort.list = this.typ.asInstanceOf[Sort.list]
    this === Const.nil(typ)
  }

  def isNull = {
    val typ: Sort.pointer = this.typ.asInstanceOf[Sort.pointer]
    this === Const._null(typ)
  }

  //Array operators

  def select(index: Expr) = {
    val typ: Sort.array = this.typ.asInstanceOf[Sort.array]
    val fun = Fun.select(typ)
    App(fun, List(this, index))
  }

  def store(index: Expr, arg: Expr) = {
    val typ: Sort.array = this.typ.asInstanceOf[Sort.array]
    val fun = Fun.store(typ)
    App(fun, List(this, index, arg))
  }

  /** "Defined as" for predicates*/
  def :<=>(that: Expr*) = {
    All.close(this <=> Expr.and(that))
  }
}

/**
 * Equality destructor
 */
object Eq {
  def apply(l: Expr, r: Expr) = {
    App(Pred._eq(l.typ), List(l, r))
  }

  def unapply(e: Expr) = e match {
    case App(fun, List(arg1, arg2)) if fun.name == Name._eq =>
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

object Expr {
  /**⋀ :n-ary and*/
  def and(xs: Iterable[Expr]): Expr = {
    if (xs.isEmpty) True
    else xs reduce (_ && _)
  }

  /**⋁:n-ary or*/
  def or(xs: Iterable[Expr]): Expr = {
    if (xs.isEmpty) False
    else xs reduce (_ || _)
  }

  def eqs(xs: Iterable[(Expr, Expr)]): Expr = {
    val zs = for ((x, y) <- xs)
      yield x === y
    and(zs)
  }
  /** zipWith (==)*/
  def eqs(xs: Iterable[Expr], ys: Iterable[Expr]): Expr = {
    assert(xs.size == ys.size)
    val zs = for ((x, y) <- (xs zip ys))
      yield x === y
    and(zs)
  }
  //Nested classes as (de)constructors from functions

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

    /**
     * extract all operands from a chain
     * of applications of the function.
     */
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

//Decorators

case class Old(expr: Expr) extends Expr {
  def typ = expr.typ
  def free = expr.free
  def rename(re: Ren) = { Old(expr rename re) }
  def subst(su: Subst) = {
    if (su.isEmpty) println("substituting in Old")
    Old(expr subst su)
  }
  def eval(st: Subst) = ???
  def eval(sts: List[Subst]) = expr eval sts.tail
  def delta = ???
}

object Old {
  def apply(exprs: List[Expr]): List[Expr] = {
    for (expr <- exprs)
      yield Old(expr)
  }

}

case class Note(expr: Expr, note: List[Any]) extends Expr { //Couldn't this be solved with a trait/mixin or something?
  def typ = expr.typ
  def free = expr.free
  def rename(re: Ren) = Note(expr rename re, note)
  def subst(su: Subst) = Note(expr subst su, note)
  def eval(st: Subst) = Note(expr eval st, note)
  def eval(sts: List[Subst]) = Note(expr eval sts, note)
  def delta = expr.delta
  override def toString = "{" + note.mkString(" ") + " " + expr + "}"
}

/**value constructor for Note*/
object Note {
  def apply(expr: Expr, notes: Any*): Note = {
    Note(expr, notes.toList)
  }
}

/**Constants, modeled as nullary functions applied to Nil*/
object Const {
  def apply(name: String, typ: Type) = {
    App(Fun(name, Nil, typ), Nil)
  }

  def unapply(expr: Expr) = expr match {
    case App(Fun.total(name, Nil, typ, _), Nil) =>
      Some((name, typ))
    case _ =>
      None
  }

  def int(n: Int) = Const(n.toString, Sort.int)
  def bool(b: Boolean) = Const(b.toString, Sort.bool)

  def nil(typ: Sort.list) = App(Fun.nil(typ), Nil)
  def _null(typ: Sort.pointer) = App(Fun._null(typ), Nil)
}

case class Var(name: Name, typ: Type) extends Expr {
  def prime = Var(name.prime, typ)
  def prefix(str: String) = Var(name prefix str, typ)
  def free = Set(this)
  def rename(re: Ren) = re getOrElse (this, this)
  def subst(su: Subst) = su getOrElse (this, this)
  def eval(st: Subst) = subst(st)
  def eval(sts: List[Subst]) = subst(sts.head)
  def fresh = Var(Var.fresh(name), typ)
  def delta = True
  override def toString = name.toString
}

object Var extends Fresh {

}

/** Application*/
case class App(fun: Fun, args: List[Expr]) extends Expr {
  assert(fun.args == args.map(_.typ), "ill-typed: " + this + ": " + fun.args + " vs " + args.map(_.typ))
  def typ = fun.ret

  def free = Set(args flatMap (_.free): _*)
  def rename(re: Ren) = App(fun, args map (_ rename re))
  def subst(su: Subst) = App(fun, args map (_ subst su))
  def eval(st: Subst) = App(fun, args map (_ eval st))
  def eval(sts: List[Subst]) = App(fun, args map (_ eval sts))

  def delta = fun match {
    case _: Fun.total =>
      Expr.and(args map (_.delta))
    case Fun.partial(name, vars, res, dom, _) =>
      val su = Subst(vars, args)
      (dom subst su) && Expr.and(args map (_.delta))
  }

  // TODO: add parens if necessary
  def format(prec: Int, assoc: Assoc): String = (fun.fixity, args) match {
    case (Nilfix, Nil) =>
      fun.toString
    case (_, List(arg1, arg2)) if fun == Pred._eq(Sort.bool) =>
      "(" + arg1 + " <=> " + arg2 + ")"
    case (_, List(Eq(arg1, arg2))) =>
      "(" + arg1 + " != " + arg2 + ")"
    case (Formfix, args) =>
      assert(fun.name.index == None)
      fun.name.text form args
    case (_: Prefix, List(arg)) =>
      fun + " " + arg
    case (_: Postfix, List(arg)) =>
      arg + " " + fun
    case (_: Infix, List(arg1, arg2)) =>
      "(" + arg1 + " " + fun + " " + arg2 + ")"
    case _ =>
      fun + args.mkString("(", ", ", ")")
  }

  override def toString = format(0, Non)
}

sealed trait Quant {
  /** Bind all free variables in ϕ*/
  def close(body: Expr, trigger: Set[Expr] = Set()): Expr = {
    val xs = body.free
    if (xs.isEmpty) body
    else Bind(this, xs, body, trigger)
  }

  def apply(bound: Iterable[Var], body: Expr, trigger: Set[Expr] = Set()): Expr = {
    apply(bound.toSet, body, trigger)
  }

  def apply(bound: Set[Var], body: Expr, trigger: Set[Expr]): Expr = {
    val xs = bound & body.free
    if (xs.isEmpty) body
    else body match {
      case Bind(q, ys, body, trigger_) if q == this =>
        Bind(this, xs ++ ys, body, trigger ++ trigger_)
      case _ =>
        Bind(this, xs, body, trigger)
    }
  }
}

case object All extends Quant {
  override def toString = "forall"
}

case object Ex extends Quant {
  override def toString = "exists"
}

case class Bind(quant: Quant, bound: Set[Var], body: Expr, trigger: Set[Expr]) extends Expr {
  assert(!bound.isEmpty)
  assert(body.typ == Sort.bool)
  def typ = Sort.bool
  def free = body.free -- bound
  def delta = Bind(quant, bound, body.delta, Set())

  def skolem = {
    val a = Ren.fresh(bound)
    body rename a
  }

  def rename(re: Ren) = {
    val xs = bound & Subst.free(re)
    val a = Ren.fresh(xs)
    val ys = a.values
    Bind(quant, bound -- xs ++ ys, body rename a rename re, trigger map (_ rename a rename re))
  }

  def subst(su: Subst) = {
    val xs = bound & Subst.free(su)
    val a = Ren.fresh(xs)
    val ys = a.values
    Bind(quant, bound -- xs ++ ys, body rename a subst su, trigger map (_ rename a subst su))
  }

  def eval(st: Subst) = {
    val xs = bound & Subst.free(st)
    val a = Ren.fresh(xs)
    val ys = a.values
    val zs = bound -- xs ++ ys
    Bind(quant, bound -- xs ++ ys, body rename a eval (st -- zs), trigger map (_ rename a eval (st -- zs)))
  }

  def eval(sts: List[Subst]) = {
    val avoid = Set(sts flatMap Subst.free: _*)
    val xs = bound & avoid
    val a = Ren.fresh(xs)
    val ys = a.values
    val zs = bound -- xs ++ ys
    Bind(quant, bound -- xs ++ ys, body rename a eval (sts map (_ -- zs)), trigger map (_ rename a eval (sts map (_ -- zs))))
  }

  override def toString = {
    "(" + quant + bound.mkString(" ", ", ", ". ") + body + ")"
  }
}