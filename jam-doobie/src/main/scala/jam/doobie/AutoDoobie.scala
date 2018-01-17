package jam.doobie

import shapeless._
import _root_.doobie._

trait AutoDoobie { self: Doobie =>

  object WriteTypeClass extends ProductTypeClass[Write] {

    def emptyProduct: Write[HNil] = new Write[HNil] {
      implicit def write: doobie.Param[HNil] =
        doobie.util.param.Param.ParamHNil
      def fr(instance: HNil): Vector[Fragment] = Vector.empty[Fragment]
    }

    def product[H, T <: HList](ch: Write[H], ct: Write[T]): Write[H :: T] =
      new Write[H :: T] {
        implicit def write: doobie.Param[H :: T] =
          doobie.util.param.Param.ParamHList(ch.write, ct.write)
        def fr(instance: H :: T): Vector[Fragment] = instance match {
          case h :: t => ch.fr(h) ++ ct.fr(t)
        }
      }

    def project[F, G](bind: => Write[G], to: F => G, from: G => F): Write[F] =
      new Write[F] {
        implicit def write: doobie.Param[F] = {
          val c = bind.write.composite
          val cf = c.imap[F]((v: G) => from(v))((v: F) => to(v))
          new doobie.util.param.Param(cf)
        }
        def fr(instance: F): Vector[Fragment] = bind.fr(to(instance))
      }
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

    def project[F, G](instance: => Read[G],
                      to: F => G,
                      from: G => F): Read[F] = new Read[F] {
      implicit def read: doobie.Composite[F] = instance.read.imap(from)(to)
    }
  }

  object LiteralTypeClass extends ProductTypeClass[Literal] {

    def emptyProduct: Literal[HNil] = (_: HNil) => Vector.empty[Fragment]

    def product[H, T <: HList](ch: Literal[H],
                               ct: Literal[T]): Literal[H :: T] = {
      case h :: t => ch.fragment(h) ++ ct.fragment(t)
    }

    def project[F, G](bind: => Literal[G],
                      to: F => G,
                      from: G => F): Literal[F] =
      (instance: F) => bind.fragment(to(instance))
  }
}
