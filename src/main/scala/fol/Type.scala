package fol

sealed trait Type extends Type.term {
  def free: Set[Param]
  def rename(re: TRen): Type
  def subst(ty: Typing): Type
}

case class Param(name: String, index: Option[Int]) extends Type with Type.x {
  def fresh(index: Int) = Param(name, Some(index))
  override def toString = "'" + name
}

case class Sort(name: String) extends Type {
  def free = Set()
  def rename(re: TRen) = this
  def subst(ty: Typing) = this
  override def toString = name
}

case class Constr(fun: Fun, test: Fun, sels: List[Fun]) {
  def free = fun.params ++ test.params ++ sels.flatMap(_.params)
  def rename(re: TRen) = ???
  def subst(ty: Typing) = ???
  override def toString = {
    val args = sels map { fun => fun + ": " + fun.ret }
    fun.format(args, 0, Non) + " with " + test
  }
}

object Sort {
  val bool = Sort("Bool")
  val int = Sort("Int")
}
object Type extends Alpha[Type, Param] {
  case class array(dom: Type, ran: Type) extends Type {
    def free = dom.free ++ ran.free
    def rename(re: TRen) = array(dom rename re, ran rename re)
    def subst(ty: Typing) = array(dom subst ty, ran subst ty)
    override def toString = "Array<" + dom + ", " + ran + ">"
  }

  case class list(elem: Type) extends Type {
    def free = elem.free
    def rename(re: TRen) = list(elem rename re)
    def subst(ty: Typing) = list(elem subst ty)
    override def toString = "List<" + elem + ">"
  }

  case class tuple(elems: List[Type]) extends Type {
    def free = Set(elems flatMap (_.free): _*)
    def rename(re: TRen) = tuple(elems map (_ rename re))
    def subst(ty: Typing) = tuple(elems map (_ subst ty))
    override def toString = "Tuple<" + elems.mkString(", ") + ">"
  }

  case class datatype(self: Param, constrs: List[Constr]) extends Type with Type.bind {
    def bound = Set(self)
    def free = Set(constrs flatMap (_.free): _*) - self
    def rename(a: TRen, re: TRen) = datatype(self rename a, constrs map (_ rename re))
    def subst(a: TRen, ty: Typing) = datatype(self rename a, constrs map (_ subst ty))
    override def toString = "Datatype<" + self + ". " + constrs.mkString(" | ") + ">"
  }

  def instantiate(pats: List[Type], args: List[Type], env: Typing): Typing = (pats, args) match {
    case (Nil, Nil) =>
      env
    case (pat :: pats, arg :: args) =>
      instantiate(pats, args, instantiate(pat, arg, env))
    case _ =>
      assert(false, "ill-typed: " + pats + " mismatches " + args)
      env
  }

  def instantiate(pat: Type, arg: Type, env: Typing): Typing = (pat, arg) match {
    case (p: Param, _) if env contains p =>
      assert(env(p) == arg, "ill-typed: " + env(p) + " mismatches " + arg)
      env
    case (p: Param, _) =>
      env + (p -> arg)
    case (pat: Sort, arg: Sort) if pat == arg =>
      env
    case (array(patdom, patran), array(argdom, argran)) =>
      instantiate(patran, argran, instantiate(patdom, argdom, env))
    case (list(pat), list(arg)) =>
      instantiate(pat, arg, env)
    case (tuple(pats), tuple(args)) =>
      instantiate(pats, args, env)
    case _ =>
      assert(pat == arg, "ill-typed: " + pat + " mismatches " + arg)
      env
  }
}