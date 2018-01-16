package jam.doobie

import shapeless._
import _root_.doobie._

trait AutoDoobie { self: Doobie =>

  object WriteTypeClass extends ProductTypeClass[BackendWrite] {

    def emptyProduct: BackendWrite[HNil] = new BackendWrite[HNil] {
      implicit def write: doobie.Param[HNil]   = doobie.util.param.Param.ParamHNil
      def fr(instance: HNil): Vector[Fragment] = Vector.empty[Fragment]
    }

    def product[H, T <: HList](ch: BackendWrite[H], ct: BackendWrite[T]): BackendWrite[H :: T] = new BackendWrite[H :: T] {
      implicit def write: doobie.Param[H :: T] = doobie.util.param.Param.ParamHList(ch.write, ct.write)
      def fr(instance: H :: T): Vector[Fragment] = instance match {
        case h :: t => ch.fr(h) ++ ct.fr(t)
      }
    }

    def project[F, G](bind: => BackendWrite[G], to: F => G, from: G => F): BackendWrite[F] = new BackendWrite[F] {
      implicit def write: doobie.Param[F] = {
        val c  = bind.write.composite
        val cf = c.imap[F]((v: G) => from(v))((v: F) => to(v))
        new doobie.util.param.Param(cf)
      }
      def fr(instance: F): Vector[Fragment] = bind.fr(to(instance))
    }
  }

  object ReadTypeClass extends ProductTypeClass[BackendRead] {
    def emptyProduct: BackendRead[HNil] = new BackendRead[HNil] {
      implicit def read: doobie.Composite[HNil] = doobie.util.composite.Composite.emptyProduct
    }

    def product[H, T <: HList](ch: BackendRead[H], ct: BackendRead[T]): BackendRead[H :: T] = new BackendRead[H :: T] {
      implicit def read: doobie.Composite[::[H, T]] = doobie.util.composite.Composite.product(ch.read, ct.read)
    }

    def project[F, G](instance: => BackendRead[G], to: F => G, from: G => F): BackendRead[F] = new BackendRead[F] {
      implicit def read: doobie.Composite[F] = instance.read.imap(from)(to)
    }
  }

  object LiteralTypeClass extends ProductTypeClass[BackendLiteral] {

    def emptyProduct: BackendLiteral[HNil] = (_: HNil) => Vector.empty[Fragment]

    def product[H, T <: HList](ch: BackendLiteral[H], ct: BackendLiteral[T]): BackendLiteral[H :: T] = {
      case h :: t => ch.fragment(h) ++ ct.fragment(t)
    }

    def project[F, G](bind: => BackendLiteral[G], to: F => G, from: G => F): BackendLiteral[F] =
      (instance: F) => bind.fragment(to(instance))
  }
}
