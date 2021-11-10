package pure

object Test {
  val a = Univ.param("a")

  val univ = Univ(
    Map(
      "Bool" -> Univ.con("Bool", 0),
      "Int" -> Univ.con("Int", 0),
      "List" -> Univ.con("List", 1)
    )
  )

  val bool = univ.sort("Bool", List())
  val list_a = univ.sort("List", List(a))
  val int = univ.sort("Int", List())
  val list_int = univ.sort("List", List(int))
  val list_bool = univ.sort("List", List(bool))

  val eqn = Sig.fun("=", List(a), List(a, a), bool)
  val nil = Sig.fun("nil", List(a), List(), list_a)
  val cons = Sig.fun("cons", List(a), List(a, list_a), list_a)

  val sig = Sig(
    Map(
      "=" -> eqn,
      "nil" -> nil,
      "cons" -> cons
    )
  )

  def main(args: Array[String]) {
    val xs = sig.formal("xs", list_int)
    val ys = sig.formal("ys", list_bool)
    val zs = sig.formal("zs", list_a)
    val vars = Map("xs" -> xs, "ys" -> ys, "zs" -> zs)

    val su0 = Map[Param, Type]()
    val Typed(expr1, typ1, su1) = sig.app("=", List(xs, zs))(vars, su0)
    val Typed(expr2, typ2, su2) = sig.app("=", List(ys, zs))(vars, su1)
  }
}
