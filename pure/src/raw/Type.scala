package raw

sealed trait Type

case class Param(name: String) extends Type
case class Sort(name: String, args: List[Type]) extends Type
