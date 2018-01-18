package jam.sql

import cats.Contravariant
import cats.data.Kleisli
import shapeless._

trait WriteTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait Write[A] { thisWrite =>
    implicit def write: W[A]
    def fr(instance: A): Vector[Fr]
    def contramap[B](f: B => A)(implicit F: Contravariant[W]): Write[B] =
      new Write[B] {
        implicit def write: W[B]        = F.contramap(thisWrite.write)(f)
        def fr(instance: B): Vector[Fr] = self.fragment(f(instance))(thisWrite.write)
      }
  }
  def writeTypeClass: ProductTypeClass[Write]
  def fragment[A: W](value: A): Vector[Fr]

  trait WriteLowPriorityInstances {

    implicit def write[A](implicit ev: W[A]): Write[A] = new Write[A] {
      def write: W[A]                 = ev
      def fr(instance: A): Vector[Fr] = self.fragment(instance)
    }

  }
  object Write extends ProductTypeClassCompanion[Write] with WriteLowPriorityInstances {
    val typeClass: ProductTypeClass[Write] = writeTypeClass

    def apply[A](implicit ev: Write[A]): Write[A] = ev

    def instance[A](implicit w: W[A]): Write[A] = new Write[A] {
      def write: W[A]                 = w
      def fr(instance: A): Vector[Fr] = self.fragment(instance)
    }

    @inline implicit def deriveWriteHNil: Write[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveWriteHCons[H, T <: HList](implicit ch: Lazy[Write[H]], ct: Lazy[Write[T]]): Write[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveWriteInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[Write[G]]): Write[F] =
      typeClass.project(cg.value, gen.to, gen.from)
  }

  implicit class BindNodeOps[A](a: A) {
    def param(implicit ev: Write[A]): BindNode[A] = BindNode(a, ev)
  }

  implicit def writeToEncode[A](implicit w: Write[A]): Encode[A] =
    Kleisli[BindExpression, A, A](a => BindNode[A](a, w))

}
