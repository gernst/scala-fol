package fol.test

import fol._

object Expr {
  val elem = Sort("Elem")
  val list = Type.list(elem)

  val x = Var("x", elem)
  val nil = Const.nil(list)
  val xs = x :: nil

  def main(args: Array[String]) {
    println(xs)
    println(xs.isNil)
  }
}