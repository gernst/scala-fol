package pure

sealed trait Type extends Type.term {}

object Type extends Alpha[Type, Param] {
  def unify(typ1: Type, typ2: Type, su: Map[Param, Type]): Map[Param, Type] = {
    (typ1, typ2) match {
      case _ if typ1 == typ2 =>
        su
      case (p1: Param, _) if su contains p1 =>
        unify(su(p1), typ2, su)
      case (_, p2: Param) if su contains p2 =>
        unify(typ1, su(p2), su)
      case (p1: Param, _) =>
        if (p1 in typ2) {
          println("recursive unification " + p1 + " in " + typ2)
          fail
        }
        su + (p1 -> typ2)
      case (_, p2: Param) =>
        unify(p2, typ1, su)
      case (Sort(con1, args1), Sort(con2, args2)) if con1 == con2 =>
        unify(args1, args2, su)
      case _ =>
        println("cannot unify " + typ1 + " and " + typ2)
        fail
    }
  }

  def unify(
      types1: List[Type],
      types2: List[Type],
      su: Map[Param, Type]
  ): Map[Param, Type] = {
    (types1, types2) match {
      case (Nil, Nil) =>
        su
      case (typ1 :: types1, typ2 :: types2) =>
        unify(types1, types2, unify(typ1, typ2, su))
      case _ =>
        println("cannot unify " + types1 + " and " + types2)
        fail
    }
  }
}

class ParamList(params: List[Param]) extends Type.xs(params)
class TypeList(types: List[Type]) extends Type.terms(types)

case class Param(name: String, index: Option[Int] = None)
    extends Type
    with Type.x {
  def fresh(index: Int) =
    Param(name, Some(index))

  def in(typ: Type): Boolean = {
    typ match {
      case that: Param =>
        return this == that
      case Sort(_, args) =>
        args exists (this in _)
    }
  }

  override def toString = index match {
    case None        => name
    case Some(index) => name + "#" + index
  }
}

case class Con(name: String, arity: Int) {
  override def toString = name + "/" + arity
}

object Con {
  val bool = Con("Bool", 0)
}

case class Sort(con: Con, args: List[Type]) extends Type {
  def free: Set[Param] = args.free
  def rename(re: Map[Param, Param]): Type =
    Sort(con, args rename re)
  def subst(su: Map[Param, Type]): Type =
    Sort(con, args subst su)

  override def toString =
    if (args.isEmpty)
      con.name
    else
      con.name + args.mkString("[", ", ", "]")
}

object Sort {
  val bool = Sort(Con.bool, Nil)
}
