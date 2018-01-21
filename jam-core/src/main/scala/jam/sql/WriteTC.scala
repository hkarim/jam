package jam.sql

import cats.data.Kleisli
import shapeless._

trait WriteTC[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  trait Write[A] { thisWrite =>
    def fr(instance: A): Vector[Fr]

    def contramap[B](f: B => A): Write[B] =
      instance => thisWrite.fr(f(instance))
  }
  def writeTypeClass: ProductTypeClass[Write]
  def fragment[A: W](value: A): Vector[Fr]

  trait WriteLowPriorityInstances {

    implicit def write[A](implicit ev: W[A]): Write[A] =
      instance => self.fragment(instance)

  }

  type WriteOption[A] = Write[Option[A]]

  trait WriteOptionInstances {

    @inline implicit def deriveWriteOptionHNil: WriteOption[HNil] = _ => Vector.empty[Fr]

    @inline implicit def deriveWriteOptionHCons1[H, T <: HList](implicit ch: Lazy[WriteOption[H]],
                                                                ct: Lazy[WriteOption[T]]): WriteOption[H :: T] = {
      case Some(h :: t) => ch.value.fr(Some(h)) ++ ct.value.fr(Some(t))
      case None         => ch.value.fr(None) ++ ct.value.fr(None)
    }

    @inline implicit def deriveWriteOptionHCons2[H, T <: HList](implicit ch: Lazy[WriteOption[H]],
                                                                ct: Lazy[WriteOption[T]]): WriteOption[Option[H] :: T] = {
      case Some(Some(h) :: t) => ch.value.fr(Some(h)) ++ ct.value.fr(Some(t))
      case Some(None :: t)    => ch.value.fr(None) ++ ct.value.fr(Some(t))
      case None               => ch.value.fr(None) ++ ct.value.fr(None)
    }

    @inline implicit def deriveWriteOptionInstance[F, G](implicit g: Generic.Aux[F, G], cg: Lazy[WriteOption[G]]): WriteOption[F] = {
      case Some(f) => cg.value.fr(Some(g.to(f)))
      case None    => cg.value.fr(None)
    }

  }

  object Write extends WriteLowPriorityInstances with ProductTypeClassCompanion[Write] with WriteOptionInstances {
    val typeClass: ProductTypeClass[Write] = writeTypeClass

    def apply[A](implicit ev: Write[A]): Write[A] = ev

    def instance[A](implicit w: W[A]): Write[A] = instance => self.fragment(instance)

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
