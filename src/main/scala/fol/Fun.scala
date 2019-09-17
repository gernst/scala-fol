package fol

case class Fun(name: String, args: List[Type], ret: Type, fixity: Fixity = Nilfix) {
  def params = Set(args flatMap (_.free): _*)
  def apply(args: Expr*) = App(this, args.toList)
  override def toString = name.toString

  // TODO: add parens only when necessary
  def format(arg: List[Any], prec: Int, assoc: Assoc): String = (fixity, args) match {
    case (Nilfix, Nil) =>
      toString
    case (_, List(arg1, arg2)) if this == Pred._eq(Sort.bool) =>
      "(" + arg1 + " <=> " + arg2 + ")"
    //    case (_, List(Eq(arg1, arg2))) =>
    //      "(" + arg1 + " != " + arg2 + ")"
    case (Formfix, args) =>
      assert(name.index == None)
      name.text form args
    case (_: Prefix, List(arg)) =>
      this + " " + arg
    case (_: Postfix, List(arg)) =>
      arg + " " + this
    case (_: Infix, List(arg1, arg2)) =>
      "(" + arg1 + " " + this + " " + arg2 + ")"
    case _ =>
      this + args.mkString("(", ", ", ")")
  }
}

object Fun {
  val uminus = Fun("-", List(Sort.int), Sort.int, Prefix(8))
  val plus = Fun("+", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))
  val minus = Fun("-", List(Sort.int, Sort.int), Sort.int, Infix(Left, 7))
  val times = Fun("*", List(Sort.int, Sort.int), Sort.int, Infix(Left, 8))
  val exp = Fun("^", List(Sort.int, Sort.int), Sort.int, Infix(Left, 9))
  lazy val divBy = Fun("/", List(Sort.int, Sort.int), Sort.int, Infix(Non, 8))
  lazy val mod = Fun("%", List(Sort.int, Sort.int), Sort.int, Infix(Non, 8))

  //list functions

  def nil(list: Type.list) = {
    Fun(Name.nil, List(), list)
  }

  def cons(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.cons, List(elem, list), list)
  }

  def in(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.in, List(elem, list), Sort.bool)
  }

  def head(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.head, List(list), elem, Nilfix)
  }

  def last(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.last, List(list), elem, Nilfix)
  }

  def init(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.init, List(list), list, Nilfix)
  }

  def tail(list: Type.list) = {
    val Type.list(elem) = list
    Fun(Name.tail, List(list), list, Nilfix)
  }

  //Array type-functions

  /**
   * Function to generate a select function for any array type
   * @param arr The Type of array for which to generate a select function
   * @return a select fuction arr -> dom -> ran
   */
  def select(arr: Type.array) = {
    val Type.array(dom, ran) = arr
    Fun(Name.select, List(arr, dom), ran, Formfix)
  }

  /**
   * Function to generate a store function for any array type
   * @param arr The Type of array for which to generate a store function
   * @return a store fuction arr -> dom -> ran -> arr
   */
  def store(arr: Type.array) = {
    val Type.array(dom, ran) = arr
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
  def apply(name: String, args: List[Type], fixity: Fixity = Nilfix): Fun = {
    Fun(name, args, Sort.bool, fixity)
  }

  def unapply(fun: Fun) = fun match {
    case Fun(name, args, Sort.bool, _) =>
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
