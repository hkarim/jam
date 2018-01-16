package jam.sql

import shapeless._

trait EntityPropertyList[H] {
  type E
  def entity: Entity[E]
  def propertyList: PropertyList[H]
}

trait Properties[A] {
  type I <: HList
  type O <: HList
  def generic: Generic.Aux[A, I]
  def validator: Validator.Aux[I, O]
  def value: O
  def vector: Vector[Property[_]]
}

trait ToPropertyVector[A] {
  def apply(a: A): Vector[Property[_]]
}
object ToPropertyVector {
  def instance[A](f: A => Vector[Property[_]]): ToPropertyVector[A]       = (a: A) => f(a)
  implicit def property[A]: ToPropertyVector[Property[A]]                 = instance[Property[A]](a => Vector(a))
  implicit def composite[C[_] <: Composite[_], A]: ToPropertyVector[C[A]] = instance[C[A]](c => c.properties.vector)
}
object ToPropertyVectorPloy extends Poly1 {
  implicit def property[A]: Case.Aux[(ToPropertyVector[A], A), Vector[Property[_]]] = at[(ToPropertyVector[A], A)] {
    case (isp, a) => isp(a)
  }
}

trait Validator[L <: HList] {
  type Out <: HList
}
object Validator {
  type Aux[I <: HList, O] = Validator[I] { type Out = O }

  implicit def hnil: Aux[HNil, HNil] = new Validator[HNil] { type Out = HNil }

  implicit def hlistProperty[InH, InT <: HList, OutT <: HList]: Aux[InH :: InT, Property[InH] :: OutT] = new Validator[InH :: InT] {
    type Out = Property[InH] :: OutT
  }
  implicit def hlistComposite[InH, C[_] <: Composite[_], InT <: HList, OutT <: HList]: Aux[InH :: InT, C[InH] :: OutT] =
    new Validator[InH :: InT] {
      type Out = C[InH] :: OutT
    }
}

trait Equatable[L[_], R[_]] {
  def eq[A](l: L[A], r: R[A]): Expression[Boolean]
  def neq[A](l: L[A], r: R[A]): Expression[Boolean]
}
trait EquatableLowPriorityInstances {
  implicit def swap[L[_], R[_]](implicit ev: Equatable[R, L]): Equatable[L, R] = new Equatable[L, R] {
    def eq[A](l: L[A], r: R[A]): Expression[Boolean]  = ev.eq(r, l)
    def neq[A](l: L[A], r: R[A]): Expression[Boolean] = ev.neq(r, l)
  }
}
object Equatable extends EquatableLowPriorityInstances {

  implicit def property[P[_] <: Property[_], E[_] <: Expression[_]]: Equatable[P, E] = new Equatable[P, E] {
    def eq[A](l: P[A], r: E[A]): Expression[Boolean]  = Eq(l, r)
    def neq[A](l: P[A], r: E[A]): Expression[Boolean] = Ne(l, r)
  }

  implicit def composite[C[_] <: Composite[_], E[_] <: Expression[_]]: Equatable[C, E] = new Equatable[C, E] {
    def eq[A](l: C[A], r: E[A]): Expression[Boolean]  = CEq(l, r)
    def neq[A](l: C[A], r: E[A]): Expression[Boolean] = CNe(l, r)
  }

  implicit def alias[N[_] <: PropertyAliasNode[_], E[_] <: Expression[_]]: Equatable[N, E] = new Equatable[N, E] {
    def eq[A](l: N[A], r: E[A]): Expression[Boolean]  = Eq(l, r)
    def neq[A](l: N[A], r: E[A]): Expression[Boolean] = Ne(l, r)
  }

}

trait ValueType[A, B] {
  def generic: Generic.Aux[A, B :: HNil]
  def valueOf(a: A): B     = generic.to(a).head
  def toValueType(b: B): A = generic.from(b :: HNil)
}

trait Comparable[A] {
  def gt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean]
  def ge[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean]
  def lt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean]
  def le[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean]
}
trait ComparableLowPriorityInstances {
  implicit def numeric[A: Numeric]: Comparable[A] = new Comparable[A] {
    def gt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Gt(l, r)
    def ge[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Ge(l, r)
    def lt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Lt(l, r)
    def le[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Le(l, r)
  }
}
object Comparable extends ComparableLowPriorityInstances {
  def apply[A](ev: Comparable[A]): Comparable[A] = ev
  def instance[A, B: Comparable](implicit g: Generic.Aux[A, B :: HNil]): Comparable[A] = new Comparable[A] with ValueType[A, B] {
    def generic: Generic.Aux[A, B :: HNil]                                                      = g // just to get rid of the compiler warning
    def gt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Gt(l, r)
    def ge[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Ge(l, r)
    def lt[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Lt(l, r)
    def le[L[_] <: Expression[_], R[_] <: Expression[_]](l: L[A], r: R[A]): Expression[Boolean] = Le(l, r)
  }
}

trait Write[A] {
  def apply(a: A): Expression[A]
}
object Write {
  def apply[A](implicit ev: Write[A]): Write[A] = ev
}

trait Literal[A] {
  def apply(a: A): Expression[A]
}
object Literal {
  def apply[A](implicit ev: Literal[A]): Literal[A] = ev
}

trait Read[F[_], A] {
  def apply(n: DQLNode[A]): F[Vector[A]]
}
object Read {
  def apply[F[_], A](implicit ev: Read[F, A]): Read[F, A] = ev
}

trait NamingStrategy {
  def name(p: Property[_]): String
  def name(e: Entity[_]): String
}
object NamingStrategy {
  object MySQL extends NamingStrategy {
    def name(e: Entity[_]): String   = s"`${e.entityName}`"
    def name(p: Property[_]): String = s"`${p.name}`"
  }
  object Postgres extends NamingStrategy {
    def name(e: Entity[_]): String   = s""""${e.entityName}""""
    def name(p: Property[_]): String = s""""${p.name}""""
  }
}
