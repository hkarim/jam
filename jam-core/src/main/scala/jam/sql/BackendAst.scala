package jam.sql

trait BackendAst[DBF[_], R[_], W[_]] { self: Backend[DBF, R, W] =>

  case class BindNode[A](instance: A, write: Write[A])        extends BindExpression[A]
  case class LiteralNode[A](instance: A, literal: Literal[A]) extends LiteralExpression[A]

}
