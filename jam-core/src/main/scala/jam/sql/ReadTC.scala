package jam.sql

import cats.{Functor, Invariant}
import shapeless._

trait ReadTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait Read[A] { self =>
    implicit def read: R[A]
    def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[Read]): Read[B] =
      F.imap(self)(f)(g)
    def map[B](f: A => B)(implicit F: Functor[R]): Read[B] = new Read[B] {
      implicit def read: R[B] = F.map(self.read)(f)
    }
  }
  def readTypeClass: ProductTypeClass[Read]

  trait ReadLowPriorityInstances {
    implicit def read[A](implicit ev: R[A]): Read[A] = new Read[A] {
      def read: R[A] = ev
    }

  }

  object Read extends ProductTypeClassCompanion[Read] with ReadLowPriorityInstances {
    val typeClass: ProductTypeClass[Read] = readTypeClass

    def apply[A](implicit ev: Read[A]): Read[A] = ev

    def instance[A](implicit r: R[A]): Read[A] = new Read[A] {
      implicit def read: R[A] = r
    }

    @inline implicit def deriveReadHNil: Read[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveReadHCons[H, T <: HList](implicit ch: Lazy[Read[H]], ct: Lazy[Read[T]]): Read[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveReadInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[Read[G]]): Read[F] =
      typeClass.project(cg.value, gen.to, gen.from)
  }

}
