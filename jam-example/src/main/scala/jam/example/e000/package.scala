package jam.example

/*
import jam.slick.implicits._
import shapeless.ops.coproduct.ZipWith
import shapeless.{Generic, HList, Poly1}
import shapeless.ops.hlist.{LiftAll, Mapper}
 */

package object e000 {

  /*
  object extractFragments extends Poly1 {
    implicit def fr[A]: Case.Aux[Write[Option[A]], Option[A] => Vector[Fr]] = at[Write[Option[A]]](_.fr)
  }

  type WriteOption[A] = Write[Option[A]]

  implicit def writeOption[A: Write, L <: HList, I <: HList, Frs]
  (implicit
    g: Generic.Aux[A, L],
    la: LiftAll.Aux[WriteOption, L, I],
    efr: Mapper.Aux[extractFragments.type, I, Frs],
    zfr: ZipWith[L, Frs]): Write[Option[A]] = new Write[Option[A]] {

    def fr(instance: Option[A]): Vector[Fr] = instance match {
      case Some(a) => Write[A].fr(a)
      case None    => Vector.empty[Fr]
    }
  }

 */

}
