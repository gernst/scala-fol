

package object fol {
  object Def {
    case class pure(lhs: Expr, rhs: Expr, cond: Option[Expr] = None) {
      def axiom = All.close(lhs === rhs)
    }
  }

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
  implicit def toName(text: String) = Name(text, None)

  case class Name(text: String, index: Option[Int]) {
    def prime = Name(text + "'", index)
    def prefix(str: String) = Name(str + text, index)
    override def toString = text __ index
  }

  object Name {
    val _eq = "=="
    val ite = "_?_:_"
    val nil = "nil"
    val _null = "null"
    val cons = "cons"
    val in = "in"
    val head = "head"
    val last = "last"
    val tail = "tail"
    val init = "init"
    val select = "_[_]"
    val store = "_[_:=_]"
  }

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
    /** A∩B=∅*/
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

}