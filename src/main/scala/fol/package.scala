

package object fol {
  type TRen = Map[Param, Param]
  
  type Typing = Map[Param, Type]
  object Typing { val empty: Typing = Map() }

  type Ren = Map[Var, Var]
  object Ren { val empty: Ren = Map() }

  type Subst = Map[Var, Expr]
  object Subst { val empty: Subst = Map() }

  val True = Const.bool(true)
  val False = Const.bool(false)

  implicit def toConst(n: Int) = Const.int(n)

  val sub = "₀₁₂₃₄₅₆₇₈₉"
  implicit class StringOps(self: String) {
    def prime = self + "'"

    def __(index: Int): String = {
      self + (index.toString map (n => sub(n - '0')))
    }

    def __(index: Option[Int]): String = index match {
      case None => self
      case Some(index) => this __ index
    }

    def form(args: List[_]): String = {
      var as = args
      val res = new StringBuilder
      for (c <- self) {
        if (c == '_') {
          res append as.head
          as = as.tail
        } else {
          res append c
        }
      }
      assert(as.isEmpty)
      res.toString
    }
  }

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

}