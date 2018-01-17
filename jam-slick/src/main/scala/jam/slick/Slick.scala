package jam.slick

import cats.{Contravariant, Functor, Invariant, Monad, Monoid}
import jam.sql._
import shapeless._
import _root_.slick.dbio.DBIO
import _root_.slick.jdbc.{GetResult, SetParameter}

import scala.concurrent.ExecutionContext

trait Slick
    extends Backend[DBIO, GetResult, SetParameter]
    with AutoSlick
    with SlickSyntax { self =>

  type Fr = Fragment

  def writeTypeClass: ProductTypeClass[Write] = WriteTypeClass
  def readTypeClass: ProductTypeClass[Read] = ReadTypeClass
  def literalTypeClass: ProductTypeClass[Literal] = LiteralTypeClass
  def fragment[A: SetParameter](value: A): Vector[Fr] = Vector(sql"$value")

  implicit object readInvariant extends Invariant[Read] {
    def imap[A, B](fa: Read[A])(f: A => B)(g: B => A): Read[B] = new Read[B] {
      def read: GetResult[B] = GetResult[B](r => f(r.<<[A](fa.read)))
    }
  }

  implicit def dbioMonad[E](implicit ec: ExecutionContext): Monad[DBIO] =
    new Monad[DBIO] {

      def pure[A](x: A): DBIO[A] = DBIO.successful(x)

      def flatMap[A, B](fa: DBIO[A])(f: A => DBIO[B]): DBIO[B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: (A) => DBIO[Either[A, B]]): DBIO[B] =
        f(a).flatMap {
          case Left(other) => tailRecM(other)(f)
          case Right(b) => DBIO.successful(b)
        }
    }

  implicit object FragmentMonoid extends Monoid[Fr] {
    def empty: Fragment = sql""
    def combine(x: Fragment, y: Fragment): Fragment = concat(x, y)
  }

  implicit object GetResultFunctor extends Functor[GetResult] {
    def map[A, B](fa: GetResult[A])(f: A => B): GetResult[B] = fa.andThen(f)
  }

  implicit object SetParameterContravariant
      extends Contravariant[SetParameter] {
    def contramap[A, B](fa: SetParameter[A])(f: B => A): SetParameter[B] =
      SetParameter((v, pp) => pp.>>(f(v))(fa))
  }

  def const(value: String): Fr = sql"#$value"

  def query[A: Read](n: Expression[A])(
      implicit ns: NamingStrategy): DBIO[Vector[A]] =
    run(n).as[A](Read[A].read)
  def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): DBIO[Int] =
    run(n).asUpdate

}
