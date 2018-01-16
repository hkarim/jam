package jam.sql

import cats.{Functor, Invariant}
import jam.data.Iso
import shapeless._

trait ReadTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait BackendRead[A] { self =>
    implicit def read: R[A]
    def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[BackendRead]): BackendRead[B] = F.imap(self)(f)(g)
    def map[B](f: A => B)(implicit F: Functor[R]): BackendRead[B] = new BackendRead[B] {
      implicit def read: R[B] = F.map(self.read)(f)
    }
  }
  def readTypeClass: ProductTypeClass[BackendRead]

  trait ReadLowPriorityInstances {
    implicit def read[A](implicit ev: R[A]): BackendRead[A] = new BackendRead[A] {
      def read: R[A] = ev
    }

    implicit def isoL[LHS, RHS](implicit ev: Iso[LHS, RHS], r: Lazy[BackendRead[RHS]], F: Functor[R]): BackendRead[LHS] =
      BackendRead[RHS](r.value).map(ev.isoFrom)
  }

  object BackendRead extends ProductTypeClassCompanion[BackendRead] with ReadLowPriorityInstances {
    val typeClass: ProductTypeClass[BackendRead] = readTypeClass

    def apply[A](implicit ev: BackendRead[A]): BackendRead[A] = ev

    def instance[A](implicit r: R[A]): BackendRead[A] = new BackendRead[A] {
      implicit def read: R[A] = r
    }

    @inline implicit def deriveReadHNil: BackendRead[HNil] =
      typeClass.emptyProduct

    @inline implicit def deriveReadHCons[H, T <: HList](implicit ch: Lazy[BackendRead[H]], ct: Lazy[BackendRead[T]]): BackendRead[H :: T] =
      typeClass.product(ch.value, ct.value)

    @inline implicit def deriveReadInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[BackendRead[G]]): BackendRead[F] =
      typeClass.project(cg.value, gen.to, gen.from)
  }

}
