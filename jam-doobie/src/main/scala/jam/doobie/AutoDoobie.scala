package jam.doobie

import shapeless._
import _root_.doobie._

trait AutoDoobie { self: Doobie =>

  object WriteTypeClass extends ProductTypeClass[Write] {

    def emptyProduct: Write[HNil] = _ => Vector.empty[Fragment]

    def product[H, T <: HList](ch: Write[H], ct: Write[T]): Write[H :: T] = {
      case h :: t => ch.fr(h) ++ ct.fr(t)
    }

    def project[F, G](bind: => Write[G], to: F => G, from: G => F): Write[F] =
      instance => bind.fr(to(instance))
  }

  object ReadTypeClass extends ProductTypeClass[Read] {
    def emptyProduct: Read[HNil] = new Read[HNil] {
      implicit def read: doobie.Composite[HNil] =
        doobie.util.composite.Composite.emptyProduct
    }

    def product[H, T <: HList](ch: Read[H], ct: Read[T]): Read[H :: T] =
      new Read[H :: T] {
        implicit def read: doobie.Composite[::[H, T]] =
          doobie.util.composite.Composite.product(ch.read, ct.read)
      }

    def project[F, G](instance: => Read[G], to: F => G, from: G => F): Read[F] = new Read[F] {
      implicit def read: doobie.Composite[F] = instance.read.imap(from)(to)
    }
  }

  object LiteralTypeClass extends ProductTypeClass[Literal] {

    def emptyProduct: Literal[HNil] = (_: HNil) => Vector.empty[Fragment]

    def product[H, T <: HList](ch: Literal[H], ct: Literal[T]): Literal[H :: T] = {
      case h :: t => ch.fr(h) ++ ct.fr(t)
    }

    def project[F, G](bind: => Literal[G], to: F => G, from: G => F): Literal[F] =
      (instance: F) => bind.fr(to(instance))
  }
}
