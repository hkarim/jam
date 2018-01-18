package jam.slick

import shapeless._
import _root_.slick.jdbc.{GetResult, SetParameter}

trait AutoSlick { self: Slick =>

  object GetResultTypeClass extends ProductTypeClass[GetResult] {

    def emptyProduct: GetResult[HNil] = GetResult[HNil](_ => HNil)

    def product[H, T <: HList](ch: GetResult[H], ct: GetResult[T]): GetResult[H :: T] =
      GetResult[H :: T](r => r.<<[H](ch) :: r.<<[T](ct))

    def project[F, G](instance: => GetResult[G], to: F => G, from: G => F): GetResult[F] =
      instance andThen from
  }

  object SetParameterTypeClass extends ProductTypeClass[SetParameter] {

    def emptyProduct: SetParameter[HNil] = SetParameter[HNil]((_, _) => ())

    def product[H, T <: HList](ch: SetParameter[H], ct: SetParameter[T]): SetParameter[H :: T] =
      SetParameter[H :: T] { (v, pp) =>
        v match {
          case h :: t =>
            pp.>>(h)(ch)
            pp.>>(t)(ct)
        }
      }

    def project[F, G](instance: => SetParameter[G], to: F => G, from: G => F): SetParameter[F] =
      SetParameter[F] { (vf, _) =>
        instance.applied(to(vf))
        ()
      }
  }

  object WriteTypeClass extends ProductTypeClass[Write] {

    def emptyProduct: Write[HNil] = new Write[HNil] {
      implicit def write: SetParameter[HNil]   = SetParameter[HNil]((_, _) => ())
      def fr(instance: HNil): Vector[Fragment] = Vector.empty[Fragment]
    }

    def product[H, T <: HList](ch: Write[H], ct: Write[T]): Write[H :: T] =
      new Write[H :: T] {
        implicit def write: SetParameter[H :: T] =
          SetParameterTypeClass.product[H, T](ch.write, ct.write)
        def fr(instance: H :: T): Vector[Fragment] = instance match {
          case h :: t => ch.fr(h) ++ ct.fr(t)
        }
      }

    def project[F, G](bind: => Write[G], to: F => G, from: G => F): Write[F] =
      new Write[F] {
        implicit def write: SetParameter[F] =
          SetParameterTypeClass.project(bind.write, to, from)
        def fr(instance: F): Vector[Fragment] = bind.fr(to(instance))
      }
  }

  object ReadTypeClass extends ProductTypeClass[Read] {
    def emptyProduct: Read[HNil] = new Read[HNil] {
      implicit def read: GetResult[HNil] = GetResult[HNil](_ => HNil)
    }

    def product[H, T <: HList](ch: Read[H], ct: Read[T]): Read[H :: T] =
      new Read[H :: T] {
        implicit def read: GetResult[H :: T] =
          GetResultTypeClass.product[H, T](ch.read, ct.read)
      }

    def project[F, G](unbind: => Read[G], to: F => G, from: G => F): Read[F] =
      new Read[F] {
        implicit def read: GetResult[F] =
          GetResultTypeClass.project(unbind.read, to, from)
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

  trait Auto[A, L] {
    val g: Generic.Aux[A, L]
  }

  trait SlickAutoGetResult[A, L] extends Auto[A, L] {
    def gr(implicit ev: GetResult[L]): GetResult[A] =
      GetResult[A](pr => g.from(pr.<<[L]))
  }

  trait SlickAutoSetParameter[A, L] extends Auto[A, L] {
    def sp(implicit ev: SetParameter[L]): SetParameter[A] = SetParameter[A] { (v, pp) =>
      pp >> g.to(v)
    }
  }

  case class Derive[P, L](g: Generic.Aux[P, L]) extends SlickAutoGetResult[P, L] with SlickAutoSetParameter[P, L]

  @inline def derive[P](implicit g: Generic[P]): Derive[P, g.Repr] =
    Derive[P, g.Repr](g)

  object GetResultCompanion extends ProductTypeClassCompanion[GetResult] {
    val typeClass: ProductTypeClass[GetResult] = GetResultTypeClass
  }

  object SetParameterCompanion extends ProductTypeClassCompanion[SetParameter] {
    val typeClass: ProductTypeClass[SetParameter] = SetParameterTypeClass
  }

  @inline implicit def deriveGRHNil: GetResult[HNil] =
    GetResultCompanion.typeClass.emptyProduct

  @inline implicit def deriveSPHNil: SetParameter[HNil] =
    SetParameterCompanion.typeClass.emptyProduct

  @inline implicit def deriveGRHCons[H, T <: HList](implicit ch: Lazy[GetResult[H]], ct: Lazy[GetResult[T]]): GetResult[H :: T] =
    GetResultCompanion.typeClass.product(ch.value, ct.value)

  @inline implicit def deriveSPHCons[H, T <: HList](implicit ch: Lazy[SetParameter[H]], ct: Lazy[SetParameter[T]]): SetParameter[H :: T] =
    SetParameterCompanion.typeClass.product(ch.value, ct.value)

  @inline implicit def deriveGRInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[GetResult[G]]): GetResult[F] =
    GetResultCompanion.typeClass.project(cg.value, gen.to, gen.from)

  @inline implicit def deriveSPInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[SetParameter[G]]): SetParameter[F] =
    SetParameterCompanion.typeClass.project(cg.value, gen.to, gen.from)

}
