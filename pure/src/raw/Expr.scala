package raw

sealed trait Expr

case class Id(name: String) extends Expr

case class App(fun: String, args: List[Expr]) extends Expr

case class Formal(name: String, typ: Type)

case class Bind(quant: String, formals: List[Formal], body: Expr) extends Expr