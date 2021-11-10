package pure

sealed trait Type extends Type.term {}

object Type extends Alpha[Type, Param] {}

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
  override def toString = name
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
      con.toString
    else
      con + args.mkString("[", ", ", "]")
}

object Sort {
  val bool = Sort(Con.bool, Nil)
}
