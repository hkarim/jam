package jam.data

import java.util.UUID

import cats.Functor
import cats.instances.option._
import shapeless._

trait Iso[L, R] {
  def isoTo(l: L): R
  def isoFrom(r: R): L
  def compose[A](that: Iso[A, L]): Iso[A, R] = Iso.compose(this, that)
  def swap: Iso[R, L] = Iso.instance[R, L](isoFrom)(isoTo)
  def liftF[F[_]: Functor]: Iso[F[L], F[R]] =
    Iso.liftF[F, L, R](Functor[F], this)
}

trait IsoLowPriorityInstances {

  def valueType[L <: AnyVal, R](
      implicit g: Generic.Aux[L, R :: HNil]): Iso[L, R] =
    Iso.instance[L, R](a => g.to(a).head)(v => g.from(v :: HNil))

  implicit def isoString[A <: AnyVal](
      implicit g: Generic.Aux[A, String :: HNil]): Iso[A, String] =
    valueType[A, String]

  implicit def isoInt[A <: AnyVal](
      implicit g: Generic.Aux[A, Int :: HNil]): Iso[A, Int] =
    valueType[A, Int]

  implicit def isoShort[A <: AnyVal](
      implicit g: Generic.Aux[A, Short :: HNil]): Iso[A, Short] =
    valueType[A, Short]

  implicit def isoLong[A <: AnyVal](
      implicit g: Generic.Aux[A, Long :: HNil]): Iso[A, Long] =
    valueType[A, Long]

  implicit def isoFloat[A <: AnyVal](
      implicit g: Generic.Aux[A, Float :: HNil]): Iso[A, Float] =
    valueType[A, Float]

  implicit def isoDouble[A <: AnyVal](
      implicit g: Generic.Aux[A, Double :: HNil]): Iso[A, Double] =
    valueType[A, Double]

  implicit def isoBoolean[A <: AnyVal](
      implicit g: Generic.Aux[A, Boolean :: HNil]): Iso[A, Boolean] =
    valueType[A, Boolean]

  implicit def isoUUID[A <: AnyVal](
      implicit g: Generic.Aux[A, UUID :: HNil]): Iso[A, UUID] =
    valueType[A, UUID]
}

trait IsoInstances extends IsoLowPriorityInstances {
  implicit val isoUUIDString: Iso[UUID, String] =
    Iso.instance[UUID, String](_.toString)(UUID.fromString)
}

object Iso extends IsoInstances {

  def apply[L, R](implicit iso: Iso[L, R]): Iso[L, R] = iso

  def instance[L, R](f: L => R)(g: R => L): Iso[L, R] = new Iso[L, R] {
    def isoTo(l: L): R = f(l)
    def isoFrom(r: R): L = g(r)
  }

  def compose[A, B, C](f: Iso[B, C], g: Iso[A, B]): Iso[A, C] =
    Iso.instance[A, C](a => f.isoTo(g.isoTo(a)))(c => g.isoFrom(f.isoFrom(c)))

  def liftF[F[_]: Functor, L, R](implicit iso: Iso[L, R]): Iso[F[L], F[R]] =
    Iso.instance[F[L], F[R]](fl => Functor[F].map(fl)(iso.isoTo))(fr =>
      Functor[F].map(fr)(iso.isoFrom))

  implicit def isoOption[L, R](
      implicit iso: Iso[L, R]): Iso[Option[L], Option[R]] =
    liftF[Option, L, R]

}
