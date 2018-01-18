package jam.sql

import cats.Monoid
import cats.data.Kleisli
import cats.syntax.monoid._

trait Backend[DBF[_], R[_], W[_]]
    extends BackendAst[DBF, R, W]
    with IsoInstances[DBF, R, W]
    with WriteTC[DBF, R, W]
    with ReadTC[DBF, R, W]
    with LiteralTC[DBF, R, W]
    with BackendSyntax[DBF, R, W] {

  type Fr

  def query[A: Read](n: Expression[A])(implicit ns: NamingStrategy): DBF[Vector[A]]
  def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): DBF[Int]

  def const(value: String): Fr
  def comma: Fr = const(",")

  implicit def read[A: Read](implicit ns: NamingStrategy): Decode[DBF, A] =
    Kleisli[DBF, DQLNode[A], Vector[A]](query)

  implicit def backendEffect: Jam[DBF] = new Jam[DBF] {
    def query[A](n: DQLNode[A])(implicit r: Decode[DBF, A], ns: NamingStrategy): DBF[Vector[A]] = r(n)
    def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): DBF[Int] =
      update(n)
  }

  def run(n: Node)(implicit ns: NamingStrategy, M: Monoid[Fr]): Fr = n match {
    case EntityName(e)               => const(ns.name(e))
    case PropertyName(p)             => const(ns.name(p))
    case PropertyAliasNode(a, v)     => const(s"${a.name}.") |+| run(PropertyName(v))
    case SubstitutedExpression(a, _) => const(a.name)
    case EncloseExpression(v)        => const("(") |+| run(v) |+| const(")")
    case AsNode(a, v, t) =>
      v match {
        case _: DQLNode[_] =>
          const("(") |+| run(t(v)) |+| const(")") |+| const(" as ") |+| const(a.name)
        case _ =>
          run(t(v)) |+| const(" as ") |+| const(a.name)
      }
    case p: Property[_]                => run(PropertyName(p))
    case c: Composite[_]               => c.properties.vector.map(PropertyName).map(run).sep(comma)
    case BindNode(v)                   => v.sep(comma)
    case LiteralNode(v)                => v.sep(comma)
    case PropertyList(ps)              => ps.map(p => run(PropertyName(p))).sep(comma)
    case EntityPropertyListNode(e, pl) => run(EntityName(e)) |+| run(pl).enclose

    case SetPropertyNode(p, v) =>
      v match {
        case _: DQLNode[_] =>
          run(PropertyName(p)) |+| const("=") |+| run(v).enclose
        case _ => run(PropertyName(p)) |+| const("=") |+| run(v)
      }

    case SetCompositeNode(c, BindNode(frs)) =>
      c.properties.vector
        .zip(frs)
        .map { case (p, f) => run(PropertyName(p)) |+| const("=") |+| f }
        .sep(comma)
    case SetCompositeNode(c, LiteralNode(frs)) =>
      c.properties.vector
        .zip(frs)
        .map { case (p, f) => run(PropertyName(p)) |+| const("=") |+| f }
        .sep(comma)

    case FunctionNode(name, e) => const(name) |+| run(e).enclose
    case ExpressionList(es)    => es.map(run).sep(comma)

    case FromNode(DQL, vs, t)                       => const("from ") |+| vs.map(v => run(t(v))).sep(comma)
    case JoinNode(p, JoinType.InnerJoin, v, t)      => run(p) |+| const(" inner join ") |+| run(t(v))
    case JoinNode(p, JoinType.LeftOuterJoin, v, t)  => run(p) |+| const(" left outer join ") |+| run(t(v))
    case JoinNode(p, JoinType.RightOuterJoin, v, t) => run(p) |+| const(" right outer join ") |+| run(t(v))
    case JoinNode(p, JoinType.CrossJoin, v, t)      => run(p) |+| const(" cross join ") |+| run(t(v))
    case OnNode(p, v, t)                            => run(p) |+| const(" on ") |+| run(t(v)).enclose
    case DQLWhereNode(p, v, t)                      => run(p) |+| const(" where ") |+| run(t(v))
    case GroupByNode(p, vs, t)                      => run(p) |+| const(" group by ") |+| vs.map(v => run(t(v))).sep(comma)
    case HavingNode(p, v, t)                        => run(p) |+| const(" having ") |+| run(t(v))
    case OrderByNode(p, vs, t)                      => run(p) |+| const(" order by ") |+| vs.map(v => run(t(v))).sep(comma)
    case LimitNode(p, v, t)                         => run(p) |+| const(" limit ") |+| run(t(v))
    case SelectNode(DQL, v, t)                      => const("select ") |+| run(t(v))
    case SelectNode(p, v, t)                        => const("select ") |+| run(t(v)) |+| const(" ") |+| run(p)

    case UnionNode(p, UnionType.Union, v, t)    => run(p) |+| const(" union ") |+| run(t(v))
    case UnionNode(p, UnionType.UnionAll, v, t) => run(p) |+| const(" union all ") |+| run(t(v))

    case DeleteFromNode(DML, v, t) => const("delete from ") |+| run(t(v))

    case InsertIntoNode(DML, v, t) => const("insert into ") |+| run(t(v))
    case ValuesNode(p, vs, t)      => run(p) |+| const(" values ") |+| vs.map(v => run(t(v)).enclose).sep(comma)
    case InsertIntoSelect(p, sn)   => run(p) |+| const(" ") |+| run(sn)

    case UpdateNode(DML, v, t) => const("update ") |+| run(t(v))
    case SetNode(p, vs, t)     => run(p) |+| const(" set ") |+| vs.map(v => run(t(v))).sep(comma)

    case DMLWhereNode(p, v, t) => run(p) |+| const(" where ") |+| run(t(v))

    case ArithmeticOperator.Plus     => const(" + ")
    case ArithmeticOperator.Minus    => const(" - ")
    case ArithmeticOperator.Multiply => const(" * ")
    case ArithmeticOperator.Divide   => const(" / ")

    case EqOperator.Eq => const(" = ")
    case EqOperator.Ne => const(" <> ")

    case PartialOrderOperator.Gt => const(" > ")
    case PartialOrderOperator.Ge => const(" >= ")
    case PartialOrderOperator.Lt => const(" < ")
    case PartialOrderOperator.Le => const(" <= ")

    case LogicOperator.And => const(" and ")
    case LogicOperator.Or  => const(" or ")

    case InfixNode(op @ LogicOperator.And, l, r) => run(l).enclose |+| run(op) |+| run(r).enclose
    case InfixNode(op @ LogicOperator.Or, l, r)  => run(l).enclose |+| run(op) |+| run(r).enclose
    case InfixNode(op, l, r)                     => run(l) |+| run(op) |+| run(r)

    case NotNode(e) => const("not ") |+| run(e).enclose

    case InNode(l, r, not) =>
      (l, r.toList) match {
        case (ExpressionList(_) | PropertyList(_), (n: DQLNode[_]) :: Nil) =>
          run(l).enclose |+| (if (not) const(" not in ") else const(" in ")) |+| run(n).enclose
        case (ExpressionList(_) | PropertyList(_), _) =>
          run(l).enclose |+| (if (not) const(" not in ") else const(" in ")) |+| r
            .map(v => run(v).enclose)
            .sep(comma)
            .enclose
        case _ =>
          run(l) |+| (if (not) const(" not in ") else const(" in ")) |+| r
            .map(run)
            .sep(comma)
            .enclose
      }

    case LikeNode(l, r, not) =>
      run(l) |+| (if (not) const(" not like ") else const(" like ")) |+| run(r)

    case IsNullNode(v, not) =>
      run(v) |+| (if (not) const(" is not null") else const(" is null"))

    case AscNode(e)  => run(e) |+| const(" asc")
    case DescNode(e) => run(e) |+| const(" desc")

  }

}
