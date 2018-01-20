package jam.sql

import cats.Functor
import jam.data.Iso
import shapeless.Lazy

trait IsoInstances[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  implicit def isoRead[LHS, RHS](implicit ev: Iso[LHS, RHS], r: Lazy[Read[RHS]], F: Functor[R]): Read[LHS] =
    Read[RHS](r.value).map(ev.isoFrom)

  implicit def isoWrite[LHS, RHS](implicit ev: Iso[LHS, RHS], w: Lazy[Write[RHS]]): Write[LHS] =
    Write[RHS](w.value).contramap[LHS](ev.isoTo)

  implicit def isoLiteral[LHS, RHS](implicit ev: Iso[LHS, RHS], l: Lazy[Literal[RHS]]): Literal[LHS] =
    Literal[RHS](l.value).contramap[LHS](ev.isoTo)

}
