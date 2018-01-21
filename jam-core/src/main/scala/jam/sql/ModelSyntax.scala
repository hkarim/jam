package jam.sql

import shapeless._
import shapeless.ops.hlist._

trait ModelSyntax {

  type VP = Vector[Property[_]]

  implicit class EncloseExpressionOps[E[_] <: Expression[_], A](e: E[A]) {
    def enclose: EncloseExpression[E, A] = EncloseExpression[E, A](e)
  }

  implicit class ExpressionOps[A](l: Expression[A]) {

    def in(first: Expression[A], rest: Expression[A]*): Expression[Boolean] =
      InNode(l, first +: rest, negate = false)
    def notIn(first: Expression[A], rest: Expression[A]*): Expression[Boolean] =
      InNode(l, first +: rest, negate = true)

    def +(r: Expression[A]): Expression[A] =
      InfixNode[A, A, A](ArithmeticOperator.Plus, l, r)
    def -(r: Expression[A]): Expression[A] =
      InfixNode[A, A, A](ArithmeticOperator.Minus, l, r)
    def *(r: Expression[A]): Expression[A] =
      InfixNode[A, A, A](ArithmeticOperator.Multiply, l, r)
    def /(r: Expression[A]): Expression[A] =
      InfixNode[A, A, A](ArithmeticOperator.Divide, l, r)

    def ===(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](EqOperator.Eq, l, r)
    def =!=(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](EqOperator.Ne, l, r)

    def >(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](PartialOrderOperator.Gt, l, r)
    def >=(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](PartialOrderOperator.Ge, l, r)
    def <(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](PartialOrderOperator.Lt, l, r)
    def <=(r: Expression[A]): Expression[Boolean] =
      InfixNode[A, A, Boolean](PartialOrderOperator.Le, l, r)

  }

  implicit class BooleanOps(l: Expression[Boolean]) {
    def and(r: Expression[Boolean]): Expression[Boolean] =
      InfixNode[Boolean, Boolean, Boolean](BinaryLogicOperator.And, l, r)
    def or(r: Expression[Boolean]): Expression[Boolean] =
      InfixNode[Boolean, Boolean, Boolean](BinaryLogicOperator.Or, l, r)
  }
  def not(e: Expression[Boolean]): Expression[Boolean] = NotNode(e)

  implicit class PropertiesSyntax[In <: HList, Out <: HList, LOut <: HList, ZOut <: HList](out: Out) {
    def properties[A](implicit g: Generic.Aux[A, In],
                      m: MappedAttribute.Aux[In, Out],
                      la: LiftAll.Aux[ToPropertyVector, Out, LOut],
                      z: Zip.Aux[LOut :: Out :: HNil, ZOut],
                      mf: MapFolder[ZOut, VP, ToPropertyVectorPloy.type]): Properties[A] =
      new Properties[A] {
        val vector: VP = (m, g, la)._3.instances
          .zip(out)
          .foldMap(Vector.empty[Property[_]])(ToPropertyVectorPloy)(_ ++ _)

      }
  }

  implicit class SymbolOps(alias: Symbol) {
    def ~[A](p: Property[A]): PropertyAliasNode[A] =
      PropertyAliasNode(alias, p)
  }

  implicit class PropertyListOps[R <: HList](r: PropertyList[R]) {
    def ::[L](l: Property[L]): PropertyList[L :: R] =
      PropertyList[L :: R](l +: r.properties)
  }

  implicit class LiftExpression[R](r: Expression[R]) {
    def ::[L](l: Expression[L]): ExpressionList[L :: R :: HNil] =
      ExpressionList[L :: R :: HNil](Vector(l, r))
  }
  implicit class LiftedExpression[R <: HList](r: ExpressionList[R]) {
    def ::[L](l: Expression[L]): ExpressionList[L :: R] =
      ExpressionList[L :: R](l +: r.expressions)
  }

  implicit class LiftProperty[E, R](r: Property[R]) {
    def ::[L](l: Property[L]): PropertyList[L :: R :: HNil] =
      PropertyList[L :: R :: HNil](Vector(l, r))
    def ::[L](l: Expression[L]): ExpressionList[L :: R :: HNil] =
      ExpressionList[L :: R :: HNil](Vector(l, r))
  }

  //implicit class EntityIsJoin[A](e: Entity[A]) extends Expression[A] with HasJoin
  implicit class AliasedSyntax[E](aliased: Aliased[E]) extends Node with HasJoin {
    override def innerJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A] =
      JoinNode(AsNode[Entity, E](Symbol(aliased.aliasName), aliased.aliasedEntity, TAs.entity[E]), JoinType.InnerJoin, a, ev)

    override def leftOuterJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A] =
      JoinNode(AsNode[Entity, E](Symbol(aliased.aliasName), aliased.aliasedEntity, TAs.entity[E]), JoinType.LeftOuterJoin, a, ev)

    override def rightOuterJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A] =
      JoinNode(AsNode[Entity, E](Symbol(aliased.aliasName), aliased.aliasedEntity, TAs.entity[E]), JoinType.RightOuterJoin, a, ev)

    override def crossJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A] =
      JoinNode(AsNode[Entity, E](Symbol(aliased.aliasName), aliased.aliasedEntity, TAs.entity[E]), JoinType.CrossJoin, a, ev)
  }

  implicit class EntityOps[A](self: Entity[A]) {
    def of[P](p: Property[P]): EntityPropertyList[P] =
      new EntityPropertyList[P] {
        type E = A
        val entity: Entity[E]             = self
        val propertyList: PropertyList[P] = PropertyList(Vector(p))
      }

    def of[C](c: Composite[C]): EntityPropertyList[C] =
      new EntityPropertyList[C] {
        type E = A
        val entity: Entity[E]             = self
        val propertyList: PropertyList[C] = PropertyList(c.properties.vector)
      }

    def of[H](pl: PropertyList[H]): EntityPropertyList[H] =
      new EntityPropertyList[H] {
        type E = A
        val entity: Entity[E]             = self
        val propertyList: PropertyList[H] = pl
      }

    def as(alias: Symbol)(implicit ev: TAs[Entity, A]): AsNode[Entity, A] =
      AsNode(alias, self, ev)

    def *[H](implicit g: Generic.Aux[A, H]): PropertyList[g.Repr] =
      PropertyList[H](self.properties.vector)
  }

  implicit def entity[E, H](implicit g: Generic.Aux[E, H]): TInsertInto[Entity, E] =
    (e: Entity[E]) => EntityPropertyListNode[E, H](e, e.*)

  trait PropertyLikeSyntax[A] {
    def lhs: Expression[A]

    def isNull: Expression[Boolean]    = IsNullNode(lhs, negate = false)
    def isNotNull: Expression[Boolean] = IsNullNode(lhs, negate = true)

    def like(e: Expression[A]): Expression[Boolean]    = LikeNode(lhs, e, negate = false)
    def notLike(e: Expression[A]): Expression[Boolean] = LikeNode(lhs, e, negate = true)

    def between(x: Expression[A], y: Expression[A]): Expression[Boolean]    = BetweenNode(lhs, x, y, negate = false)
    def notBetween(x: Expression[A], y: Expression[A]): Expression[Boolean] = BetweenNode(lhs, x, y, negate = true)

    def asc: OrderLikeNode[A]  = AscNode(lhs)
    def desc: OrderLikeNode[A] = DescNode(lhs)
  }

  implicit class PropertyOps[P](p: Property[P]) extends PropertyLikeSyntax[P] {
    def lhs: Expression[P]                       = p
    def :=(e: Expression[P]): SetPropertyNode[P] = SetPropertyNode[P](p, e)
    def as(alias: Symbol)(implicit ev: TAs[Property, P]): AsNode[Property, P] =
      AsNode(alias, p, ev)
  }

  implicit class PropertyAliasOps[A](pa: PropertyAliasNode[A]) extends PropertyLikeSyntax[A] {
    def lhs: Expression[A] = pa
  }

  implicit class CompositeOps[C](c: Composite[C]) {
    def :=(e: Expression[C]): SetCompositeNode[C] = SetCompositeNode[C](c, e)

  }

}
