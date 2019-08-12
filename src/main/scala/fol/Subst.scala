package fol

/** Inherited by objects for which a fresh name can be generated*/
trait Fresh {
  var _index = 0

  def next = {
    _index += 1
    _index
  }

  def fresh(name: Name) = {
    val Name(text, _) = name
    val index = next
    Name(text, Some(index))
  }
}

object Ren {
  def fresh(xs: Iterable[Var]): Ren = {
    val ys = xs map (x => (x, x.fresh))
    ys.toMap
  }

  def apply(xs: Iterable[(Var, Var)]): Ren = {
    xs.toMap
  }

  def apply(xs: Iterable[Var], ys: Iterable[Var]): Ren = {
    val zs = (xs zip ys)
    zs.toMap
  }
}

object Subst {
  def empty: Ren = {
    Map.empty
  }

  def id(xs: Iterable[Var]): Ren = {
    val ys = xs map (x => (x, x))
    ys.toMap
  }

  def free(xs: Subst): Set[Var] = {
    val ys = xs.values flatMap (_.free)
    ys.toSet
  }

  def prefix(str: String, xs: Iterable[Var]): Ren = {
    val ys = xs map (x => (x, x prefix str))
    ys.toMap
  }

  def apply(xs: Iterable[(Var, Expr)]): Subst = {
    xs.toMap
  }

  def apply(xs: Iterable[Var], ys: Iterable[Expr]): Subst = {
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
