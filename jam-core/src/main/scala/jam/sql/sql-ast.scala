package jam.sql

import cats.data.NonEmptyList

trait Node

trait Expression[+A] extends Node { self =>
  def ~(alias: Symbol): Expression[A] = SubstitutedExpression(alias, self)
}
case class ExpressionList[H](expressions: Vector[Expression[_]]) extends Expression[H]

sealed trait Attribute[+A]

sealed trait Property[+P] extends Attribute[P] with Expression[P] { self =>
  def name: String
  def ? : Property[Option[P]]
}
object Property {
  case class Strict[+P](name: String) extends Property[P] {
    def ? : Strict[Option[P]] = Strict[Option[P]](name)
  }
}
case class PropertyList[H](properties: Vector[Property[_]]) extends Expression[H]

trait Composite[C] extends Attribute[C] with Expression[C] { self =>
  def widen: Composite[C] = this
  def properties: Properties[C]
}

trait Entity[A] extends Composite[A] { self =>
  def entityName: String
}

sealed trait Settable extends Node {
  type F[_]
  type T
  def settable: F[T]
  def value: Expression[T]
}
case class SetPropertyNode[A](property: Property[A], value: Expression[A]) extends Settable with Expression[A] {
  type F[_] = Property[_]
  type T    = A
  val settable: F[T] = property
}
case class SetCompositeNode[A](composite: Composite[A], value: Expression[A]) extends Settable with Expression[A] {
  type F[_] = Composite[_]
  type T    = A
  val settable: F[A] = composite
}

////////////

trait TAs[F[_], A] {
  def apply(instance: F[A]): Node
}
object TAs {
  implicit def entity[A]: TAs[Entity, A]     = (instance: Entity[A]) => EntityName(instance)
  implicit def property[A]: TAs[Property, A] = (instance: Property[A]) => PropertyName(instance)
}

trait TFrom[F[_], A] {
  def apply(instance: F[A]): Node
}
object TFrom {
  implicit def entity[E[_] <: Entity[_], A]: TFrom[E, A]     = (instance: E[A]) => EntityName(instance)
  implicit def join[F[_] <: JoinLikeNode[_], A]: TFrom[F, A] = identity(_)

}

trait TJoin[F[_], A] {
  def apply(instance: F[A]): Node
}
object TJoin {
  implicit def entity[E[_] <: Entity[_], A]: TJoin[E, A] = (instance: E[A]) => EntityName(instance)
  implicit def as[F[_] <: AsLikeNode[_], A]: TJoin[F, A] = identity(_)
}

trait TWhere[F[_], A] {
  def apply(instance: F[A]): Node
}
object TWhere {
  implicit def booleanExpression: TWhere[Expression, Boolean] = identity(_)
}

trait TGroupBy[F[_], A] {
  def apply(instance: F[A]): Node
}
object TGroupBy {
  implicit def orderingNode[O[_] <: OrderLikeNode[_], A]: TGroupBy[O, A] = identity(_)
}

trait TOrderBy[F[_], A] {
  def apply(instance: F[A]): Node
}
object TOrderBy {
  implicit def orderingNode[O[_] <: OrderLikeNode[_], A]: TOrderBy[O, A] = identity(_)
}

trait TLimit[F[_], A] {
  def apply(instance: F[A]): Node
}
object TLimit {
  implicit def expression[E[_] <: Expression[_]]: TLimit[E, Long] = identity(_)
}

trait TOffset[F[_], A] {
  def apply(instance: F[A]): Node
}
object TOffset {
  implicit def expression[E[_] <: Expression[_]]: TOffset[E, Long] = identity(_)
}

trait TSelect[F[_], A] {
  def apply(instance: F[A]): Node
}
object TSelect {
  implicit def expression[E[_] <: Expression[_], A]: TSelect[E, A] = identity(_)
  //implicit def asLike[E[_] <: AsLikeNode[_], A]: TSelect[E, A] = identity(_)
}

trait TUnion[F[_], A] {
  def apply(instance: F[A]): Node
}
object TUnion {
  implicit def unionLike[E[_] <: UnionLikeNode[_], A]: TUnion[E, A] = identity(_)
}

trait TInsertInto[F[_], A] {
  def apply(instance: F[A]): Node
}
object TInsertInto {

  implicit def entityPropertyList[A]: TInsertInto[EntityPropertyList, A] =
    (instance: EntityPropertyList[A]) => EntityPropertyListNode[instance.E, A](instance.entity, instance.propertyList)
}

trait TValues[F[_], A] {
  def apply(instance: F[A]): Node
}
object TValues {
  implicit def expression[E[_] <: Expression[_], A]: TValues[E, A] = identity(_)
}

trait TUpdate[F[_], A] {
  def apply(instance: F[A]): Node
}
object TUpdate {
  implicit def entity[A]: TUpdate[Entity, A] = (instance: Entity[A]) => EntityName(instance)
}

trait TSet[-A] {
  def apply(instance: A): Node
}
object TSet {
  implicit def settable: TSet[Settable] = identity(_)
}

///////////

trait HasFrom { self: Node =>
  def from[F[_], A](a: F[A], as: F[A]*)(implicit ev: TFrom[F, A]): FromNode[F, A] = FromNode(self, a +: as, ev)
}

trait HasJoin { self: Node =>
  def innerJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A]      = JoinNode(self, JoinType.InnerJoin, a, ev)
  def leftOuterJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A]  = JoinNode(self, JoinType.LeftOuterJoin, a, ev)
  def rightOuterJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A] = JoinNode(self, JoinType.RightOuterJoin, a, ev)
  def crossJoin[F[_], A](a: F[A])(implicit ev: TJoin[F, A]): JoinNode[F, A]      = JoinNode(self, JoinType.CrossJoin, a, ev)
}

trait HasSelect { self: Node =>
  def select[F[_], A](fa: F[A])(implicit ev: TSelect[F, A]): SelectNode[F, A] = SelectNode(self, fa, ev)
}

trait HasUnion[A] { self: Node =>
  def union[F[_]](fa: F[A])(implicit ev: TUnion[F, A]): UnionNode[F, A]    = UnionNode(self, UnionType.Union, fa, ev)
  def unionAll[F[_]](fa: F[A])(implicit ev: TUnion[F, A]): UnionNode[F, A] = UnionNode(self, UnionType.UnionAll, fa, ev)
}

trait HasDQLWhere { self: Node =>
  def where[F[_], A](fa: F[A])(implicit ev: TWhere[F, A]): DQLWhereNode[F, A] = DQLWhereNode(self, fa, ev)
}

trait HasGroupBy { self: Node =>
  def groupBy[F[_], A](a: F[A], as: F[A]*)(implicit ev: TGroupBy[F, A]): GroupByNode[F, A] = GroupByNode(self, a +: as, ev)
}

trait HasOrderBy { self: Node =>
  def orderBy[F[_], A](a: F[A], as: F[A]*)(implicit ev: TOrderBy[F, A]): OrderByNode[F, A] = OrderByNode(self, a +: as, ev)
}

trait HasLimit { self: Node =>
  def limit[F[_], A](a: F[A])(implicit ev: TLimit[F, A]): LimitNode[F, A] = LimitNode(self, a, ev)
}

trait HasInsertInto { self: Node =>
  def insertInto[F[_], A](fa: F[A])(implicit ev: TInsertInto[F, A]): InsertIntoNode[F, A] = InsertIntoNode(self, fa, ev)
}

trait HasUpdate { self: Node =>
  def update[F[_], A](fa: F[A])(implicit ev: TUpdate[F, A]): UpdateNode[F, A] = UpdateNode(self, fa, ev)
}

trait HasDeleteFrom { self: Node =>
  def deleteFrom[F[_], A](fa: F[A])(implicit ev: TFrom[F, A]): DeleteFromNode[F, A] = DeleteFromNode(self, fa, ev)
}

trait HasDMLWhere { self: Node =>
  def where[F[_], A](fa: F[A])(implicit ev: TWhere[F, A]): DMLWhereNode[F, A] = DMLWhereNode(self, fa, ev)
}

case object DQL extends Node with HasFrom with HasSelect

case object DML extends Node with HasInsertInto with HasUpdate with HasDeleteFrom

trait Where    extends Node
trait DQLWhere extends Where with HasSelect
trait DMLWhere extends Where
///////////

case class EntityName(value: Entity[_])                                  extends Node
case class PropertyName(value: Property[_])                              extends Node
case class SubstitutedExpression[A](alias: Symbol, value: Expression[A]) extends Expression[A]

case class PropertyAliasNode[A](alias: Symbol, value: Property[A]) extends Expression[A] {
  def asc: OrderLikeNode[A]  = AscNode(this)
  def desc: OrderLikeNode[A] = DescNode(this)
}

sealed trait AsLikeNode[+A]                                           extends Expression[A] with JoinLikeNode[A] with HasJoin
case class AsNode[F[_], A](alias: Symbol, value: F[A], ta: TAs[F, A]) extends AsLikeNode[A]

case class FunctionNode[A, T](name: String, args: Expression[A]) extends Expression[T]

case class EntityPropertyListNode[E, H](entity: Entity[E], propertyList: PropertyList[H]) extends Expression[H]

sealed trait OrderLikeNode[+A]           extends Node
case class AscNode[A](e: Expression[A])  extends OrderLikeNode[A] with Expression[A]
case class DescNode[A](e: Expression[A]) extends OrderLikeNode[A] with Expression[A]

///////////
trait JoinLikeNode[+A] extends Node

sealed trait JoinType
object JoinType {
  case object InnerJoin      extends JoinType
  case object LeftOuterJoin  extends JoinType
  case object RightOuterJoin extends JoinType
  case object CrossJoin      extends JoinType
}

case class JoinNode[F[_], A](parent: Node, jt: JoinType, value: F[A], tj: TJoin[F, A]) extends JoinLikeNode[A] with HasJoin {
  def on[G[_], T](fa: G[T])(implicit ev: TWhere[G, T]): OnNode[G, T] = OnNode(this, fa, ev)
}
case class OnNode[F[_], A](parent: Node, value: F[A], tw: TWhere[F, A]) extends JoinLikeNode[A] with HasJoin

sealed trait DQLNode[+A]       extends Expression[A]
sealed trait UnionLikeNode[+A] extends DQLNode[A]
sealed trait UnionType
object UnionType {
  case object Union    extends UnionType
  case object UnionAll extends UnionType
}

case class UnionNode[F[_], A](parent: Node, ut: UnionType, value: F[A], tu: TUnion[F, A]) extends UnionLikeNode[A] with HasUnion[A]

///////////

case class FromNode[F[_], A](parent: Node, value: TraversableOnce[F[A]], tf: TFrom[F, A])
    extends Node
    with HasDQLWhere
    with HasGroupBy
    with HasOrderBy
    with HasLimit
    with HasSelect

case class DQLWhereNode[F[_], A](parent: Node, value: F[A], tw: TWhere[F, A])
    extends Node
    with HasGroupBy
    with HasOrderBy
    with HasLimit
    with HasSelect

case class GroupByNode[F[_], A](parent: Node, value: TraversableOnce[F[A]], tg: TGroupBy[F, A])
    extends Node
    with HasOrderBy
    with HasLimit
    with HasSelect {
  def having[G[_], T](gt: G[T])(implicit tw: TWhere[G, T]): HavingNode[G, T] = HavingNode(this, gt, tw)
}
case class HavingNode[F[_], A](parent: Node, value: F[A], tw: TWhere[F, A]) extends Node with HasOrderBy with HasLimit with HasSelect

case class OrderByNode[F[_], A](parent: Node, value: TraversableOnce[F[A]], to: TOrderBy[F, A]) extends Node with HasLimit with HasSelect
case class LimitNode[F[_], A](parent: Node, value: F[A], tl: TLimit[F, A])                      extends Node with HasSelect
case class OffsetNode[F[_], A](parent: Node, value: F[A], to: TOffset[F, A])                    extends Node with HasSelect

case class SelectNode[F[_], A](parent: Node, value: F[A], ts: TSelect[F, A]) extends UnionLikeNode[A] with HasUnion[A] {
  def as(alias: Symbol): AsNode[Expression, A] = AsNode(alias, this: Expression[A], identity(_))
}

sealed trait DMLNode[+A]                                                      extends Expression[A]
case class DMLWhereNode[F[_], A](parent: Node, value: F[A], tw: TWhere[F, A]) extends DMLNode[A]

case class InsertIntoNode[F[_], A](parent: Node, value: F[A], ti: TInsertInto[F, A]) extends Node {
  def values[G[_]](x: G[A], xs: G[A]*)(implicit ev: TValues[G, A]): ValuesNode[G, A]     = ValuesNode(this, x +: xs, ev)
  def values[G[_]](xs: NonEmptyList[G[A]])(implicit ev: TValues[G, A]): ValuesNode[G, A] = ValuesNode(this, xs.toList, ev)
  def subQuery(sn: DQLNode[A]): InsertIntoSelect[A]                                      = InsertIntoSelect(this, sn)
}
case class ValuesNode[F[_], A](parent: Node, value: TraversableOnce[F[A]], tv: TValues[F, A]) extends DMLNode[A]
case class InsertIntoSelect[A](parent: Node, value: DQLNode[A])                               extends DMLNode[A]

case class UpdateNode[F[_], A](parent: Node, value: F[A], tu: TUpdate[F, A]) extends Node {
  def set[T](x: T, xs: T*)(implicit ev: TSet[T]): SetNode[T] = SetNode(this, x +: xs, ev)
}
case class SetNode[A](parent: Node, value: TraversableOnce[A], ts: TSet[A]) extends DMLNode[A] with HasDMLWhere

case class DeleteFromNode[F[_], A](parent: Node, value: F[A], tf: TFrom[F, A]) extends Node with HasDMLWhere

case class Eq[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class Ne[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class Lt[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class Le[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class Gt[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class Ge[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]

case class CEq[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]
case class CNe[L[_] <: Expression[_], R[_] <: Expression[_], A](lhs: L[A], rhs: R[A]) extends Expression[Boolean]

case class InNode[A](lhs: Expression[A], rhs: TraversableOnce[Expression[A]], negate: Boolean) extends Expression[Boolean]

case class Like[A](lhs: Expression[A], rhs: Expression[A]) extends Expression[Boolean]

case class And(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Or(lhs: Expression[Boolean], rhs: Expression[Boolean])  extends Expression[Boolean]

case class Not(e: Expression[Boolean]) extends Expression[Boolean]
