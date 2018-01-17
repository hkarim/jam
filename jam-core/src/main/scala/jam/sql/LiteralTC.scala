package jam.sql

import shapeless._

trait LiteralTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait Literal[A] { l =>
    def fragment(instance: A): Vector[Fr]
    def contramap[B](f: B => A): Literal[B] =
      (instance: B) => l.fragment(f(instance))
  }

  def literalTypeClass: ProductTypeClass[Literal]

  trait LiteralLowerPriorityInstances {
    implicit def option[A: Literal]: Literal[Option[A]] = {
      case Some(v) => Literal[A].fragment(v)
      case None => Vector(const("NULL"))
    }
  }

  trait LiteralLowPriorityInstances extends LiteralLowerPriorityInstances {

    implicit val string: Literal[String] = (a: String) =>
      Vector(const(s"'$a'"))
    implicit def numeric[N: Numeric]: Literal[N] =
      (a: N) => Vector(const(s"$a"))

  }

  object Literal
      extends ProductTypeClassCompanion[Literal]
      with LiteralLowPriorityInstances {
    val typeClass: ProductTypeClass[Literal] = literalTypeClass

    @inline implicit def deriveLiteralHNil: Literal[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveLiteralHCons[H, T <: HList](
        implicit ch: Lazy[Literal[H]],
        ct: Lazy[Literal[T]]): Literal[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveLiteralInstance[F, G](
        implicit gen: Generic.Aux[F, G],
        cg: Lazy[Literal[G]]): Literal[F] =
      typeClass.project(cg.value, gen.to, gen.from)

    def apply[A](implicit ev: Literal[A]): Literal[A] = ev
    def instance[A](f: A => Vector[Fr]): Literal[A] = (a: A) => f(a)
  }

  implicit class LiteralOps[A](a: A) {
    def literal(implicit ev: Literal[A]): LiteralNode[A] =
      LiteralNode(ev.fragment(a))
  }

  implicit def literalToConstant[A](implicit l: Literal[A]): Constant[A] =
    (a: A) => LiteralNode[A](l.fragment(a))

}
