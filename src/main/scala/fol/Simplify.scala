package fol

case class SimplificationFailure(reason: Any*) extends Error

object Simplify {
  val default = Simplify(Nil)

  def simplify(expr: Expr, defs: List[Def.pure]): Expr = {
    val self = Simplify(defs)
    self(expr)
  }
}

case class Context(path: List[Expr], eqs: Subst) {
  def ::(phi: Expr) = {
    Context(phi :: path, eqs)
  }

  def +(xe: (Var, Expr)) = {
    val (x, e) = xe
    assert(!(e.free contains x), "recursive equation: " + (x === e))
    Context(path, eqs + xe)
  }

  def maps(x: Var) = {
    eqs contains x
  }

  def apply(x: Var) = {
    eqs(x)
  }

  def contains(expr: Expr) = {
    path contains expr
  }

  override def toString = (path ++ eqs).mkString(" && ")
}

object Context {
  val empty = Context(List.empty, Subst.empty)
}

case class Simplify(defs: List[Def.pure]) extends (Expr => Expr) {
  def apply(phi: Expr): Expr = {
    try {
      simplify(phi, Context.empty)
    } catch {
      case _: StackOverflowError =>
        throw SimplificationFailure("nonterminating simplifcation", phi)
    }
  }

  def simplify(exprs: List[Expr], ctx: Context): List[Expr] = {
    exprs map (simplify(_, ctx))
  }

  def rewrite(exprs: List[Expr], ctx: Context): List[Expr] = {
    exprs map (rewrite(_, ctx))
  }

  def simplify(phi: Expr, ctx: Context): Expr = {
    val res = _simplify(phi, ctx)
    if (res == phi) phi // re-use of objects
    else res
  }

  def _simplify(phi: Expr, ctx: Context): Expr = phi match {
    case Expr.not(phi) =>
      !simplify(phi, ctx)

    case Expr.and(phi, psi) =>
      val (_phi, _psi) = binary(phi, true, psi, true, ctx)
      _phi && _psi

    case Expr.or(phi, psi) =>
      val (_phi, _psi) = binary(phi, false, psi, false, ctx)
      _phi || _psi

    case Expr.imp(phi, Expr.imp(psi, chi)) =>
      simplify((phi && psi) ==> chi, ctx)

    case Expr.imp(phi, psi) =>
      val (_phi, _psi) = binary(phi, true, psi, false, ctx)
      _phi ==> _psi

    case Eq(left, right) =>
      val _left = simplify(left, ctx)
      val _right = simplify(right, ctx)
      literal(_left === _right, ctx)

    case Bind(q, xs, body) =>
      val _body = simplify(body, ctx) // XXX: assumes that ctx does not extend over free variables of body
      val __body = prune(_body, q, xs, true)
      q(xs, __body)

    case _ if phi.typ == Sort.bool =>
      literal(phi, ctx)

    case _ =>
      rewrite(phi, ctx)
  }

  def rewrite(expr: App, ctx: Context, defs: List[Def.pure]): Expr = defs match {
    case Nil =>
      expr
    case Def.pure(pat, rhs, cond) :: rest =>
      bind(pat, expr, Subst.empty) match {
        case None =>
          rewrite(expr, ctx, rest)
        case Some(env) =>
          cond match {
            case None =>
              simplify(rhs subst env, ctx)
            case Some(phi) =>
              val _phi = simplify(phi subst env, ctx)
              if (_phi == True)
                simplify(rhs subst env, ctx)
              else
                rewrite(expr, ctx, rest)
          }
      }
  }

  def rewrite(expr: Expr, ctx: Context): Expr = expr match {
    case x: Var if ctx maps x =>
      rewrite(ctx(x), ctx)

    case x: Var =>
      x

    /* case App(fun, args) if (defs contains fun) =>
      val Def.pure(`fun`, xs, body) = defs(fun)
      val su = Subst(xs, args)
      rewrite(body subst su, ctx) */

    case App(fun, args) =>
      val expr = App(fun, rewrite(args, ctx))
      if (fun.name == Name._eq) {
        args match {
          case List(App(fun1, _), App(fun2, _)) if fun1.name == Name.cons && fun2.name == Name.cons =>
            println(expr)
          case _ =>
        }
      }
      rewrite(expr, ctx, defs)
  }

  def literal(phi: Expr, ctx: Context): Expr = {
    if (ctx contains False) True
    else if (ctx contains phi) True
    else if (ctx contains !phi) False
    else rewrite(phi, ctx)
  }

  def binary(
    phi: Expr, phi_pos: Boolean,
    psi: Expr, psi_pos: Boolean,
    ctx: Context,
    psi_done: Boolean = false,
    swap: Boolean = false): (Expr, Expr) =
    {
      val newctx = if (psi_pos) assume(psi, ctx) else assert(psi, ctx)
      val newphi = simplify(phi, newctx)
      val phi_done = phi == newphi

      if (phi_done && psi_done) {
        if (swap) (psi, phi)
        else (phi, psi)
      } else {
        binary(
          psi, psi_pos,
          newphi, phi_pos,
          ctx,
          phi_done, !swap)
      }
    }

  def assume(phi: Expr, ctx: Context): Context = phi match {
    case True =>
      ctx
    case Expr.not(psi) =>
      assert(psi, ctx)
    case Expr.and(phi, psi) =>
      assume(phi, assume(psi, ctx))
    case Eq(x: Var, e) if !(e.free contains x) =>
      ctx + (x -> e)
    case Eq(e, x: Var) if !(e.free contains x) =>
      assume(x === e, ctx)
    case _ =>
      phi :: ctx
  }

  def assert(phi: Expr, ctx: Context): Context = phi match {
    case False =>
      ctx
    case Expr.not(psi) =>
      assume(psi, ctx)
    case Expr.imp(phi, psi) =>
      assert(phi, assume(psi, ctx))
    case Expr.or(phi, psi) =>
      assert(phi, assert(psi, ctx))
    case _ =>
      !phi :: ctx
  }

  def assume(args: List[Expr], ctx: Context): Context = {
    args.foldRight(ctx)(assume)
  }

  def assert(args: List[Expr], ctx: Context): Context = {
    args.foldRight(ctx)(assert)
  }

  def prune(phi: Expr, q: Quant, bound: Set[Var], pos: Boolean): Expr = phi match {
    case Eq(x: Var, e) if !(e.free contains x) && (bound contains x) =>
      if (pos && q == Ex || !pos && q == All) {
        True
      } else {
        phi
      }
    case Eq(e, x: Var) if !(e.free contains x) =>
      prune(x === e, q, bound, pos)
    case Expr.not(psi) =>
      val _psi = prune(psi, q, bound, !pos)
      !_psi
    case Expr.imp(phi, psi) =>
      val _phi = prune(phi, q, bound, !pos)
      val _psi = prune(psi, q, bound, pos)
      _phi ==> _psi
    case Expr.or(phi, psi) =>
      val _phi = prune(phi, q, bound, !pos)
      val _psi = prune(psi, q, bound, !pos)
      _phi || _psi
    case Expr.and(phi, psi) =>
      val _phi = prune(phi, q, bound, pos)
      val _psi = prune(psi, q, bound, pos)
      _phi && _psi
    case _ =>
      phi
  }

  def bind(pats: List[Expr], args: List[Expr], env0: Subst): Option[Subst] = (pats, args) match {
    case (Nil, Nil) =>
      Some(env0)
    case (pat :: pats, arg :: args) =>
      for (
        env1 <- bind(pat, arg, env0);
        env2 <- bind(pats, args, env1)
      ) yield env2
    case _ =>
      None
  }

  def bind(pat: Expr, arg: Expr, env: Subst): Option[Subst] = (pat, arg) match {
    case (x: Var, _) if !(env contains x) =>
      Some(env + (x -> arg))
    case (x: Var, _) if (env contains x) && (env(x) == arg) =>
      Some(env + (x -> arg))
    case (x: Var, _) =>
      None
    case (App(fun1, pats), App(fun2, args)) if fun1 == fun2 =>
      bind(pats, args, env)
    case _ =>
      None
  }
}