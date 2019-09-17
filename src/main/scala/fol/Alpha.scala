package fol

object AlphaTest {
  sealed trait Expr extends Expr.term

  case class Var(name: String, index: Option[Int] = None) extends Expr with Expr.x {
    def fresh(index: Int) = Var(name, Some(index))
    override def toString = index match {
      case None => name
      case Some(index) => name + index
    }
  }

  case class App(fun: String, args: List[Expr]) extends Expr {
    def free = Set(args flatMap (_.free): _*)
    def rename(re: Map[Var, Var]) = App(fun, args map (_ rename re))
    def subst(su: Map[Var, Expr]) = App(fun, args map (_ subst su))
    override def toString = fun + args.mkString("(", ", ", ")")
  }

  case class Lam(params: List[Var], body: Expr) extends Expr with Expr.bind {
    def bound = params.toSet
    def free = body.free -- params
    def rename(a: Map[Var, Var], re: Map[Var, Var]): Expr = Lam(params map (_ rename a), body rename re)
    def subst(a: Map[Var, Var], su: Map[Var, Expr]): Expr = Lam(params map (_ rename a), body subst su)
    override def toString = params.mkString("\\", ", ", ". ") + body
  }

  object Expr extends Alpha[Expr, Var]

  def main(args: Array[String]) {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val x_ = Var("x'")
    val y_ = Var("y'")
    val z_ = Var("z'")

    val e = Lam(List(x, y), App("f", List(x, z)))

    val g = App("g", List(x, y, z))
    val h = App("h", List(x, y, z))
    val i = App("i", List(x, y, z))
    val re = Expr.subst(x -> x_, y -> y_, z -> z_)
    val su = Expr.subst(x -> g, y -> h, z -> i)

    println(e rename re)
    println(e subst su)
    // Expected output:
    // \x, y. f(x, z')
    // \x1, y2. f(x1, i(x, y, z))
  }
}

object Alpha {
  trait term[E, V <: E] {
    this: E =>
    def free: Set[V]
    def rename(re: Map[V, V]): E
    def subst(su: Map[V, E]): E
  }

  trait x[E, V <: E] extends term[E, V] {
    this: V =>
    def fresh(index: Int): V
    def free = Set(this)
    def rename(re: Map[V, V]) = re.getOrElse(this, this)
    def subst(su: Map[V, E]) = su.getOrElse(this, this)
  }
}

trait Alpha[E <: Alpha.term[E, V], V <: E with Alpha.x[E, V]] {
  context =>

  type term = Alpha.term[E, V]
  type x = Alpha.x[E, V]

  trait bind extends term {
    this: E =>
    def bound: Set[V]

    def rename(a: Map[V, V], re: Map[V, V]): E
    def subst(a: Map[V, V], su: Map[V, E]): E

    def avoid(xs: Set[V]) = {
      val captured = bound & xs
      context.fresh(captured)
    }

    def rename(re: Map[V, V]) = {
      val xs = context.free(re)
      val alpha = avoid(xs)
      rename(alpha, re -- bound ++ alpha)
    }

    def subst(su: Map[V, E]) = {
      val xs = context.free(su)
      val alpha = avoid(xs)
      subst(alpha, su -- bound ++ alpha)
    }
  }

  var _index = 0

  def nextIndex = {
    _index += 1
    _index
  }

  def id(xs: Iterable[V]): Map[V, V] = {
    val ys = xs map (x => (x, x))
    ys.toMap
  }

  def fresh(xs: Iterable[V]): Map[V, V] = {
    val ys = xs map (x => (x, x fresh nextIndex))
    ys.toMap
  }

  def free(xs: Map[V, E]): Set[V] = {
    val ys = xs.values flatMap (_.free)
    ys.toSet
  }

  def subst[B <: E](xs: (V, B)*): Map[V, B] = {
    xs.toMap
  }

  def subst[B <: E](xs: Iterable[(V, B)]): Map[V, B] = {
    xs.toMap
  }

  def subst[B <: E](xs: Iterable[V], ys: Iterable[B]): Map[V, B] = {
    assert(xs.size == ys.size)
    val zs = (xs zip ys)
    zs.toMap
  }

  def compose(inner: Subst, outer: Subst) = {
    val updated = inner map {
      case (x, e) => (x, e subst outer)
    }
    updated ++ outer
  }
}