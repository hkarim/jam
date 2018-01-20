package jam.example

/*
import jam.slick.implicits._
import shapeless.ops.coproduct.ZipWith
import shapeless.{Generic, HList, Poly1}
import shapeless.ops.hlist.{LiftAll, Mapper}
import slick.jdbc.SetParameter
*/

package object e000 {

  /*
  object extractSetParameter extends Poly1 {
    implicit def sp[A]: Case.Aux[Write[Option[A]], SetParameter[Option[A]]] = at[Write[Option[A]]](_.write)
  }
  object extractFragments extends Poly1 {
    implicit def fr[A]: Case.Aux[Write[Option[A]], Option[A] => Vector[Fr]] = at[Write[Option[A]]](_.fr)
  }

  type WriteOption[A] = Write[Option[A]]

  implicit def writeOption[A: Write, L <: HList, I <: HList, SPs, Frs]
  (implicit
    g: Generic.Aux[A, L],
    la: LiftAll.Aux[WriteOption, L, I],
    esp: Mapper.Aux[extractSetParameter.type, I, SPs],
    efr: Mapper.Aux[extractFragments.type, I, Frs],
    zsp: ZipWith[L, SPs],
    zfr: ZipWith[L, Frs]): Write[Option[A]] = new Write[Option[A]] {
    implicit def write: SetParameter[Option[A]] = SetParameter[Option[A]] { (v, pp) =>
      v match {
        case Some(a) => pp.>>(a)(Write[A].write)
      }

    }

    def fr(instance: Option[A]): Vector[Fr] = instance match {
      case Some(a) => Write[A].fr(a)
      case None    => Vector.empty[Fr]
    }
  }
  */



}
