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
  def instance[A](f: A => Vector[Property[_]]): ToPropertyVector[A] =
    (a: A) => f(a)
  implicit def property[A]: ToPropertyVector[Property[A]] =
    instance[Property[A]](a => Vector(a))
  implicit def composite[C[_] <: Composite[_], A]: ToPropertyVector[C[A]] =
    instance[C[A]](c => c.properties.vector)
}
object ToPropertyVectorPloy extends Poly1 {
  implicit def property[A]: Case.Aux[(ToPropertyVector[A], A), Vector[Property[_]]] =
    at[(ToPropertyVector[A], A)] {
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
