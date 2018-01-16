package jam.sql

import cats.Contravariant
import jam.data.Iso
import shapeless._

trait WriteTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait BackendWrite[A] { b =>
    implicit def write: W[A]
    def fr(instance: A): Vector[Fr]
    def contramap[B](f: B => A)(implicit F: Contravariant[W]): BackendWrite[B] = new BackendWrite[B] {
      implicit def write: W[B]        = F.contramap(b.write)(f)
      def fr(instance: B): Vector[Fr] = self.fragment(f(instance))(b.write)
    }
  }
  def writeTypeClass: ProductTypeClass[BackendWrite]
  def fragment[A: W](value: A): Vector[Fr]

  trait WriteLowPriorityInstances {

    implicit def write[A](implicit ev: W[A]): BackendWrite[A] = new BackendWrite[A] {
      def write: W[A]                 = ev
      def fr(instance: A): Vector[Fr] = self.fragment(instance)
    }

    implicit def isoL[LHS, RHS](implicit ev: Iso[LHS, RHS], w: Lazy[BackendWrite[RHS]], F: Contravariant[W]): BackendWrite[LHS] =
      BackendWrite[RHS](w.value).contramap[LHS](ev.isoTo)

  }
  object BackendWrite extends ProductTypeClassCompanion[BackendWrite] with WriteLowPriorityInstances {
    val typeClass: ProductTypeClass[BackendWrite] = writeTypeClass

    def apply[A](implicit ev: BackendWrite[A]): BackendWrite[A] = ev

    def instance[A](implicit w: W[A]): BackendWrite[A] = new BackendWrite[A] {
      def write: W[A]                 = w
      def fr(instance: A): Vector[Fr] = self.fragment(instance)
    }

    @inline implicit def deriveWriteHNil: BackendWrite[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveWriteHCons[H, T <: HList](implicit ch: Lazy[BackendWrite[H]],
                                                         ct: Lazy[BackendWrite[T]]): BackendWrite[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveWriteInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[BackendWrite[G]]): BackendWrite[F] =
      typeClass.project(cg.value, gen.to, gen.from)
  }

  implicit class BindNodeOps[A](a: A) {
    def param(implicit ev: BackendWrite[A]): BindNode[A] = BindNode(ev.fr(a))
  }

  implicit def fromBackendWriteToWrite[A](implicit w: BackendWrite[A]): Write[A] = (a: A) => BindNode[A](w.fr(a))

}
