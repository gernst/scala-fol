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
    println(univ)
    println(sig)

    {
      val xs = sig.formal("xs", list_int)
      val ys = sig.formal("ys", list_bool)
      val zs = sig.formal("zs", list_a)
      val vars = Map("xs" -> xs, "ys" -> ys, "zs" -> zs)

      val su = Map[Param, Type]()
      val (nil, su0) = sig.const("nil")(vars, su)
      val (expr1, su1) = sig.app("=", List(xs, zs))(vars, su0)
      val (expr2, su2) = sig.app("=", List(nil, zs))(vars, su1)

      println(expr1 inst su2)
      println(expr2 inst su2)
    }

    {
      object check extends Check(sig)
      object check_ extends Check(sig)

      val xs = check.x("xs", list_int)
      val ys = check.x("ys", list_bool)
      val zs = check.x("zs", list_a)

      val nil = check.const("nil")
      val expr1 = check.app("=", List(xs, zs))
      val expr2 = check.app("=", List(nil, zs))
      println(expr1.resolve)
      println(expr2.resolve)
    }
  }
}
