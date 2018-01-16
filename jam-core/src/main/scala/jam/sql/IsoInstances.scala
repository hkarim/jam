package jam.sql

import cats.{Contravariant, Functor}
import jam.data.Iso
import shapeless.Lazy

trait IsoInstances[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  implicit def isoRead[LHS, RHS](implicit ev: Iso[LHS, RHS], r: Lazy[BackendRead[RHS]], F: Functor[R]): BackendRead[LHS] =
    BackendRead[RHS](r.value).map(ev.isoFrom)

  implicit def isoWrite[LHS, RHS](implicit ev: Iso[LHS, RHS], w: Lazy[BackendWrite[RHS]], F: Contravariant[W]): BackendWrite[LHS] =
    BackendWrite[RHS](w.value).contramap[LHS](ev.isoTo)

  implicit def isoLiteral[LHS, RHS](implicit ev: Iso[LHS, RHS], l: Lazy[BackendLiteral[RHS]]): BackendLiteral[LHS] =
    BackendLiteral[RHS](l.value).contramap[LHS](ev.isoTo)

}
