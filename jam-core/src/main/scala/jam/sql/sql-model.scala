package jam.sql

import shapeless._

import scala.annotation.implicitNotFound

trait EntityPropertyList[H] {
  type E
  def entity: Entity[E]
  def propertyList: PropertyList[H]
}

trait Properties[A] { self =>
  def vector: Vector[Property[_]]
  def optional: Properties[Option[A]] = new Properties[Option[A]] {
    def vector: Vector[Property[_]] = self.vector
  }
}
object Properties {
  type Aux[A0, I0 <: HList, O0 <: HList] = Properties[A0] {
    type I = I0
    type O = O0
  }
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

trait MappedAttribute[L <: HList] extends Serializable {
  def capture: Any
  type Out <: HList
}

object MappedAttribute {
  def apply[L <: HList](implicit mapped: MappedAttribute[L]): Aux[L, mapped.Out] = mapped

  @implicitNotFound("""
     Cannot validate that the input model type matches the shape ${Out0}
     For any input model `case class M(a1: A1, ..., an: An)`,
     the required shaped must be on the form `F[A1] :: ... :: F[An] :: HNil`
     where `F` is either `Property` or `Composite`
     - If you are calling `properties` on an HList, make sure the shape of HList matches exactly the model
     - If you are using composites, consider calling `widen` on composite instance
     - Validate that the order of your properties matches exactly the model
     - If you have an optional composite, calling `optional` on composite instance might help
   """)
  type Aux[L <: HList, Out0 <: HList] = MappedAttribute[L] { type Out = Out0 }

  implicit def hnilMapped: Aux[HNil, HNil] = new MappedAttribute[HNil] {
    type Out = HNil
    def capture: Any = HNil
  }

  implicit def hlistMappedProperty[H, T <: HList, OutM <: HList](
      implicit mt: MappedAttribute.Aux[T, OutM]): Aux[H :: T, Property[H] :: OutM] =
    new MappedAttribute[H :: T] {
      type Out = Property[H] :: OutM
      def capture: Any = mt
    }

  implicit def hlistMappedComposite[H, T <: HList, OutM <: HList, C[_] <: Composite[_]](
      implicit mt: MappedAttribute.Aux[T, OutM]): Aux[H :: T, C[H] :: OutM] =
    new MappedAttribute[H :: T] {
      type Out = C[H] :: OutM
      def capture: Any = mt
    }

}

trait NamingStrategy {
  def name(p: Property[_]): String
  def name(e: Entity[_]): String
}
object NamingStrategy {
  object MySQL extends NamingStrategy {
    def name(e: Entity[_]): String = s"`${e.entityName}`"
    def name(p: Property[_]): String = p match {
      case Property.Strict(n)     => s"`$n`"
      case Property.Aliased(n, a) => s"$a.`$n`"
    }

  }
  object Postgres extends NamingStrategy {
    def name(e: Entity[_]): String = s""""${e.entityName}""""
    def name(p: Property[_]): String = p match {
      case Property.Strict(n)     => s""""$n""""
      case Property.Aliased(n, a) => s"""$a."$n""""
    }

  }
}
