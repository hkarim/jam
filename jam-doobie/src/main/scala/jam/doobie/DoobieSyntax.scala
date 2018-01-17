package jam.doobie

import jam.sql._
import shapeless.{Generic, HList, Lazy}
import _root_.doobie._
import jam.data.Iso
import scala.reflect.runtime.universe.TypeTag

trait DoobieSyntax { self: Doobie =>

  implicit def entitySyntax[E]: EntitySyntax[E] = new EntitySyntax[E] {
    def findOne(e: Entity[E])(expression: Expression[Boolean])(
        implicit r: Read[E],
        ns: NamingStrategy): ConnectionIO[Option[E]] =
      self
        .query(DQL.from(e).where(expression).limit(1L.literal).select(e))(r,
                                                                          ns)
        .map(_.headOption)

    def find(e: Entity[E])(expression: Expression[Boolean])(
        implicit r: Read[E],
        ns: NamingStrategy): ConnectionIO[Vector[E]] =
      self.query(DQL.from(e).where(expression).select(e))(r, ns)
  }

  implicit def dqlSyntax[A]: DQLSyntax[A] = new DQLSyntax[A] {
    def query(n: DQLNode[A])(implicit r: Read[A],
                             ns: NamingStrategy): ConnectionIO[Vector[A]] =
      self.query[A](n)

    def one(n: DQLNode[A])(implicit r: Read[A],
                           ns: NamingStrategy): ConnectionIO[Option[A]] =
      self.query[A](n).map(_.headOption)
  }

  implicit def dqlConversionSyntax[L <: HList]: DQLConversionSyntax[L] =
    new DQLConversionSyntax[L] {
      def to[A](n: DQLNode[L])(implicit g: Generic.Aux[A, L],
                               r: Read[L],
                               ns: NamingStrategy): ConnectionIO[Vector[A]] =
        self.query[L](n).map(ls => ls.map(g.from))
    }

  implicit def dmlSyntax[A]: DMLSyntax[A] = new DMLSyntax[A] {
    def update(n: DMLNode[A])(implicit ns: NamingStrategy): ConnectionIO[Int] =
      self.update(n)
  }

  def isoMeta[L: TypeTag, R: Meta](implicit iso: Lazy[Iso[L, R]]): Meta[L] =
    Meta[R].xmap[L](iso.value.isoFrom, iso.value.isoTo)

}
