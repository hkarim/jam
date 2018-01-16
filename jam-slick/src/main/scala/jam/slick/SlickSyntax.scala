package jam.slick

import jam.sql._
import shapeless.{Generic, HList}
import _root_.slick.dbio.DBIO
import _root_.slick.jdbc.ActionBasedSQLInterpolation

import scala.concurrent.ExecutionContext

trait SlickSyntax { self: Slick =>

  implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation = new ActionBasedSQLInterpolation(s)

  implicit def entitySyntax[E](implicit ec: ExecutionContext): EntitySyntax[E] = new EntitySyntax[E] {
    def findOne(e: Entity[E])(expression: Expression[Boolean])(implicit r: BackendRead[E], ns: NamingStrategy): DBIO[Option[E]] =
      self.query(DQL.from(e).where(expression).limit(1L.literal).select(e))(r, ns).map(_.headOption)

    def find(e: Entity[E])(expression: Expression[Boolean])(implicit r: BackendRead[E], ns: NamingStrategy): DBIO[Vector[E]] =
      self.query(DQL.from(e).where(expression).select(e))(r, ns)
  }

  implicit def dqlSyntax[A](implicit ec: ExecutionContext): DQLSyntax[A] = new DQLSyntax[A] {
    def query(n: DQLNode[A])(implicit r: BackendRead[A], ns: NamingStrategy): DBIO[Vector[A]] =
      self.query[A](n)

    def one(n: DQLNode[A])(implicit r: BackendRead[A], ns: NamingStrategy): DBIO[Option[A]] =
      self.query[A](n).map(_.headOption)
  }

  implicit def dqlConversionSyntax[L <: HList](implicit ec: ExecutionContext): DQLConversionSyntax[L] = new DQLConversionSyntax[L] {
    def to[A](n: DQLNode[L])(implicit g: Generic.Aux[A, L], r: BackendRead[L], ns: NamingStrategy): DBIO[Vector[A]] =
      self.query[L](n).map(ls => ls.map(g.from))
  }

  implicit def dmlSyntax[A]: DMLSyntax[A] = new DMLSyntax[A] {
    def update(n: DMLNode[A])(implicit ns: NamingStrategy): DBIO[Int] =
      self.update(n)
  }

}
