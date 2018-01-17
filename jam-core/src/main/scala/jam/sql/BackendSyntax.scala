package jam.sql

import cats.Monoid
import cats.syntax.monoid._
import shapeless._

trait BackendSyntax[DBF[_], R[_], W[_]] extends ModelSyntax { self: Backend[DBF, R, W] =>

  protected[this] def interleave(xs: TraversableOnce[Fr], sep: Fr)(implicit M: Monoid[Fr]): Fr = {
    var b     = const("")
    var first = true
    xs.foreach { x =>
      if (first) first = false
      else b = b |+| sep
      b = b |+| x
    }
    b
  }

  protected[this] implicit class FrOps(l: Fr)(implicit M: Monoid[Fr]) {
    //def ++(r: Fr): Fr = M.combine(l, r)
    def enclose: Fr = const("(") |+| l |+| const(")")
  }
  protected[this] implicit class FrTraversableOps(frs: TraversableOnce[Fr])(implicit M: Monoid[Fr]) {
    def sep(by: Fr): Fr = interleave(frs, by)
  }

  trait EntitySyntax[E] {
    def findOne(e: Entity[E])(expression: Expression[Boolean])(implicit r: Read[E], ns: NamingStrategy): DBF[Option[E]]
    def find(e: Entity[E])(expression: Expression[Boolean])(implicit r: Read[E], ns: NamingStrategy): DBF[Vector[E]]
  }
  object EntitySyntax {
    def apply[E](implicit ev: EntitySyntax[E]): EntitySyntax[E] = ev
  }

  implicit class EntitySyntaxOps[E: EntitySyntax](e: Entity[E]) {
    def findOne(expression: Expression[Boolean])(implicit r: Read[E], ns: NamingStrategy): DBF[Option[E]] =
      EntitySyntax[E].findOne(e)(expression)

    def find(expression: Expression[Boolean])(implicit r: Read[E], ns: NamingStrategy): DBF[Vector[E]] =
      EntitySyntax[E].find(e)(expression)
  }

  trait DQLSyntax[A] {
    def query(n: DQLNode[A])(implicit r: Read[A], ns: NamingStrategy): DBF[Vector[A]]
    def one(n: DQLNode[A])(implicit r: Read[A], ns: NamingStrategy): DBF[Option[A]]
  }
  object DQLSyntax {
    def apply[A](implicit ev: DQLSyntax[A]): DQLSyntax[A] = ev
  }

  implicit class DQLSyntaxOps[A: DQLSyntax](n: DQLNode[A]) {
    def query(implicit r: Read[A], ns: NamingStrategy): DBF[Vector[A]] =
      DQLSyntax[A].query(n)

    def one(implicit r: Read[A], ns: NamingStrategy): DBF[Option[A]] =
      DQLSyntax[A].one(n)
  }

  trait DQLConversionSyntax[L <: HList] {
    def to[A](n: DQLNode[L])(implicit g: Generic.Aux[A, L], r: Read[L], ns: NamingStrategy): DBF[Vector[A]]
  }
  object DQLConversionSyntax {
    def apply[L <: HList](implicit ev: DQLConversionSyntax[L]): DQLConversionSyntax[L] = ev
  }

  implicit class DQLConversionSyntaxOps[L <: HList: DQLConversionSyntax](n: DQLNode[L]) {
    def to[A](implicit g: Generic.Aux[A, L], r: Read[L], ns: NamingStrategy): DBF[Vector[A]] =
      DQLConversionSyntax[L].to[A](n)
  }

  trait DMLSyntax[A] {
    def update(n: DMLNode[A])(implicit ns: NamingStrategy): DBF[Int]
  }
  object DMLSyntax {
    def apply[A](implicit ev: DMLSyntax[A]): DMLSyntax[A] = ev
  }

  implicit class DMLSyntaxOps[A: DMLSyntax](n: DMLNode[A]) {
    def update(implicit ns: NamingStrategy): DBF[Int] = DMLSyntax[A].update(n)
  }

}
