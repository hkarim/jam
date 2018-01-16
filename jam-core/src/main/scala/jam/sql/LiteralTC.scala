package jam.sql

import shapeless._

trait LiteralTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait BackendLiteral[A] { l =>
    def fragment(instance: A): Vector[Fr]
    def contramap[B](f: B => A): BackendLiteral[B] = (instance: B) => l.fragment(f(instance))
  }

  def literalTypeClass: ProductTypeClass[BackendLiteral]

  trait LiteralLowerPriorityInstances {
    implicit def option[A: BackendLiteral]: BackendLiteral[Option[A]] = {
      case Some(v) => BackendLiteral[A].fragment(v)
      case None    => Vector(const("NULL"))
    }
  }

  trait LiteralLowPriorityInstances extends LiteralLowerPriorityInstances {

    implicit val string: BackendLiteral[String]         = (a: String) => Vector(const(s"'$a'"))
    implicit def numeric[N: Numeric]: BackendLiteral[N] = (a: N) => Vector(const(s"$a"))

  }

  object BackendLiteral extends ProductTypeClassCompanion[BackendLiteral] with LiteralLowPriorityInstances {
    val typeClass: ProductTypeClass[BackendLiteral] = literalTypeClass

    @inline implicit def deriveLiteralHNil: BackendLiteral[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveLiteralHCons[H, T <: HList](implicit ch: Lazy[BackendLiteral[H]],
                                                           ct: Lazy[BackendLiteral[T]]): BackendLiteral[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveLiteralInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[BackendLiteral[G]]): BackendLiteral[F] =
      typeClass.project(cg.value, gen.to, gen.from)

    def apply[A](implicit ev: BackendLiteral[A]): BackendLiteral[A] = ev
    def instance[A](f: A => Vector[Fr]): BackendLiteral[A]          = (a: A) => f(a)
  }

  implicit class LiteralOps[A](a: A) {
    def literal(implicit ev: BackendLiteral[A]): LiteralNode[A] = LiteralNode(ev.fragment(a))
  }

  implicit def fromBackendLiteralToLiteral[A](implicit l: BackendLiteral[A]): Literal[A] = (a: A) => LiteralNode[A](l.fragment(a))

}
