package jam.slick

import cats.{Contravariant, Functor, MonadError, Monoid}
import jam.sql._
import shapeless._
import _root_.slick.dbio.DBIO
import _root_.slick.jdbc.{GetResult, SetParameter}

import scala.concurrent.ExecutionContext

trait Slick extends Backend[DBIO, GetResult, SetParameter] with AutoSlick with SlickSyntax { self =>

  type Fr = Fragment

  def writeTypeClass: ProductTypeClass[Write]         = WriteTypeClass
  def readTypeClass: ProductTypeClass[Read]           = ReadTypeClass
  def literalTypeClass: ProductTypeClass[Literal]     = LiteralTypeClass
  def fragment[A: SetParameter](value: A): Vector[Fr] = Vector(sql"$value")

  implicit object FragmentMonoid extends Monoid[Fr] {
    def empty: Fragment                             = sql""
    def combine(x: Fragment, y: Fragment): Fragment = concat(x, y)
  }

  implicit object GetResultFunctor extends Functor[GetResult] {
    def map[A, B](fa: GetResult[A])(f: A => B): GetResult[B] = fa.andThen(f)
  }

  implicit object SetParameterContravariant extends Contravariant[SetParameter] {
    def contramap[A, B](fa: SetParameter[A])(f: B => A): SetParameter[B] =
      SetParameter((v, pp) => pp.>>(f(v))(fa))
  }

  implicit object ReadInvariantFunctor extends Functor[Read] {
    def map[A, B](fa: Read[A])(f: A => B): Read[B] = new Read[B] {
      implicit def read: GetResult[B] = GetResult[B](r => f(r.<<[A](fa.read)))
    }
  }
  implicit object WriteContravariant extends Contravariant[Write] {
    def contramap[A, B](fa: Write[A])(f: B => A): Write[B] =
      fa.contramap[B](f)(SetParameterContravariant)
  }
  implicit object LiteralContravariant extends Contravariant[Literal] {
    def contramap[A, B](fa: Literal[A])(f: B => A): Literal[B] =
      fa.contramap(f)
  }

  implicit def dbioMonadError(implicit ec: ExecutionContext): MonadError[DBIO, Throwable] =
    new MonadError[DBIO, Throwable] {

      def pure[A](x: A): DBIO[A] = DBIO.successful(x)

      def flatMap[A, B](fa: DBIO[A])(f: A => DBIO[B]): DBIO[B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: (A) => DBIO[Either[A, B]]): DBIO[B] =
        f(a).flatMap {
          case Left(other) => tailRecM(other)(f)
          case Right(b)    => DBIO.successful(b)
        }

      def raiseError[A](e: Throwable): DBIO[A] = DBIO.failed(e)

      def handleErrorWith[A](fa: DBIO[A])(f: Throwable => DBIO[A]): DBIO[A] = {
        def translate(to: Option[Throwable]): DBIO[A] = to match {
          case Some(t) => f(t)
          case None    => fa
        }
        fa.cleanUp(translate, keepFailure = false)
      }

    }

  def const(value: String): Fr = sql"#$value"

  def query[A: Read](n: Expression[A])(implicit ns: NamingStrategy): DBIO[Vector[A]] =
    run(n).as[A](Read[A].read)

  def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): DBIO[Int] =
    run(n).asUpdate

}
