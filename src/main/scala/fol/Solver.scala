package fol

case class ProofUnknown(reason: Any*) extends Error
case class ProofFailure(reason: Any*) extends Error
case class ProofError(reason: Any*) extends Error

object Solver {
  def default = SMT2.z3(1)
  def dummy = new DummySolver
}

class DummySolver extends Solver {
  def assume(phi: Expr) {}
  def assumeDistinct(exprs: Iterable[Expr]) {}
  def push() {}
  def pop() {}
  def isConsistent: Boolean = false
}

trait Solver {
  def assume(phi: Expr)
  def assumeDistinct(exprs: Iterable[Expr])
  def push()
  def pop()

  def isConsistent: Boolean

  def isSatisfiable(phi: Expr): Boolean = {
    assuming(phi) { isConsistent }
  }

  def assume(phis: Iterable[Expr]) {
    for (phi <- phis)
      assume(phi)
  }

  def scoped[A](f: => A): A = {
    push()
    try {
      f
    } finally {
      pop()
    }
  }

  def assuming[A](phis: Expr*)(f: => A): A = scoped {
    assume(phis)
    f
  }

  def isValid(phi: Expr): Boolean = {
    !isSatisfiable(!phi)
  }
}
