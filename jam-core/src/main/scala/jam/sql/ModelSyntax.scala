package jam.sql

import shapeless._
import shapeless.ops.hlist._

trait ModelSyntax {

  type VP = Vector[Property[_]]

  def property[P](name: String): Property[P] = Property.Strict[P](name)

  implicit class BooleanOps(l: Expression[Boolean]) {
    def and(r: Expression[Boolean]): Expression[Boolean] = And(l, r)
    def or(r: Expression[Boolean]): Expression[Boolean]  = Or(l, r)
  }
  def not(e: Expression[Boolean]): Expression[Boolean] = Not(e)

  implicit class EquatableOps[L[_], A](l: L[A]) {
    def ===[R[_]](r: R[A])(implicit ev: Equatable[L, R]): Expression[Boolean] = ev.eq(l, r)
    def =!=[R[_]](r: R[A])(implicit ev: Equatable[L, R]): Expression[Boolean] = ev.neq(l, r)
  }

  implicit class ComparableOps[L[_] <: Expression[_], A](l: L[A]) {
    def >[R[_] <: Expression[_]](r: R[A])(implicit ev: Comparable[A]): Expression[Boolean]  = ev.gt(l, r)
    def >=[R[_] <: Expression[_]](r: R[A])(implicit ev: Comparable[A]): Expression[Boolean] = ev.ge(l, r)
    def <[R[_] <: Expression[_]](r: R[A])(implicit ev: Comparable[A]): Expression[Boolean]  = ev.lt(l, r)
    def <=[R[_] <: Expression[_]](r: R[A])(implicit ev: Comparable[A]): Expression[Boolean] = ev.le(l, r)
  }

  implicit class ExpressionOps[A](l: Expression[A]) {
    def in(first: Expression[A], rest: Expression[A]*): Expression[Boolean]    = InNode(l, first +: rest, negate = false)
    def notIn(first: Expression[A], rest: Expression[A]*): Expression[Boolean] = InNode(l, first +: rest, negate = true)
    def like(e: Expression[A]): Expression[Boolean]                            = Like(l, e)
  }

  implicit class PropertiesOps[In <: HList, Out <: HList, LOut <: HList, ZOut <: HList](out: Out) {
    def properties[A](implicit g: Generic.Aux[A, In],
                      v: Validator.Aux[In, Out],
                      la: LiftAll.Aux[ToPropertyVector, Out, LOut],
                      z: Zip.Aux[LOut :: Out :: HNil, ZOut],
                      mf: MapFolder[ZOut, VP, ToPropertyVectorPloy.type]): Properties[A] = new Properties[A] {
      type I = In
      type O = Out
      val generic: Generic.Aux[A, I]     = g
      val validator: Validator.Aux[I, O] = v
      val value: O                       = out
      val vector: VP                     = la.instances.zip(out).foldMap(Vector.empty[Property[_]])(ToPropertyVectorPloy)(_ ++ _)
    }
  }

  implicit class SymbolOps(alias: Symbol) {
    def ~[A](p: Property[A]): PropertyAliasNode[A] = PropertyAliasNode(alias, p)
  }

  implicit class PropertyListOps[R <: HList](r: PropertyList[R]) {
    def ::[L](l: Property[L]): PropertyList[L :: R] = PropertyList[L :: R](l +: r.properties)
  }

  implicit class LiftExpression[R](r: Expression[R]) {
    def ::[L](l: Expression[L]): ExpressionList[L :: R :: HNil] = ExpressionList[L :: R :: HNil](Vector(l, r))
  }
  implicit class LiftedExpression[R <: HList](r: ExpressionList[R]) {
    def ::[L](l: Expression[L]): ExpressionList[L :: R] = ExpressionList[L :: R](l +: r.expressions)
  }

  implicit class LiftProperty[E, R](r: Property[R]) {
    def ::[L](l: Property[L]): PropertyList[L :: R :: HNil]     = PropertyList[L :: R :: HNil](Vector(l, r))
    def ::[L](l: Expression[L]): ExpressionList[L :: R :: HNil] = ExpressionList[L :: R :: HNil](Vector(l, r))
  }

  //implicit class EntityIsJoin[A](e: Entity[A]) extends Expression[A] with HasJoin

  implicit class EntityOps[A](self: Entity[A]) {
    def of[P](p: Property[P]): EntityPropertyList[P] = new EntityPropertyList[P] {
      type E = A
      val entity: Entity[E]             = self
      val propertyList: PropertyList[P] = PropertyList(Vector(p))
    }

    def of[C](c: Composite[C]): EntityPropertyList[C] = new EntityPropertyList[C] {
      type E = A
      val entity: Entity[E]             = self
      val propertyList: PropertyList[C] = PropertyList(c.properties.vector)
    }

    def of[H](pl: PropertyList[H]): EntityPropertyList[H] = new EntityPropertyList[H] {
      type E = A
      val entity: Entity[E]             = self
      val propertyList: PropertyList[H] = pl
    }

    def as(alias: Symbol)(implicit ev: TAs[Entity, A]): AsNode[Entity, A] = AsNode(alias, self, ev)

    def *[H](implicit g: Generic.Aux[A, H]): PropertyList[g.Repr] = PropertyList[H](self.properties.vector)
  }

  implicit def entity[E, H](implicit g: Generic.Aux[E, H]): TInsertInto[Entity, E] =
    (e: Entity[E]) => EntityPropertyListNode[E, H](e, e.*)

  implicit class PropertyOps[P](p: Property[P]) {
    def :=(e: Expression[P]): SetPropertyNode[P]                              = SetPropertyNode[P](p, e)
    def asc: OrderLikeNode[P]                                                 = AscNode(p)
    def desc: OrderLikeNode[P]                                                = DescNode(p)
    def as(alias: Symbol)(implicit ev: TAs[Property, P]): AsNode[Property, P] = AsNode(alias, p, ev)
  }

  implicit class CompositeOps[C](c: Composite[C]) {
    def :=(e: Expression[C]): SetCompositeNode[C] = SetCompositeNode[C](c, e)

  }

}
