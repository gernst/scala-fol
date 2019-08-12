package fol

sealed trait Type

case class Param(name: String) extends Type {

}

case class Sort(name: String) extends Type {
  override def toString = name
}

case class Constr(fun: Fun, test: Fun, sels: List[Fun])

object Sort {
  val bool = Sort("Bool")
  val int = Sort("Int")

  case class pointer(elem: Type) extends Type {
    override def toString = "Pointer<" + elem + ">"
  }

  case class array(dom: Type, ran: Type) extends Type {
    override def toString = "Array<" + dom + ", " + ran + ">"
  }

  case class list(elem: Type) extends Type {
    override def toString = "List<" + elem + ">"
  }

  case class tuple(elems: List[Type]) extends Type {
    override def toString = "Tuple<" + elems.mkString(", ") + ">"
  }

  case class datatype(name: String, constrs: List[Constr]) extends Type {
    ???
  }
}
