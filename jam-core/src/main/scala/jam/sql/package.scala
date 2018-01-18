package jam

import cats.Monad
import cats.data.Kleisli
import shapeless._

package object sql {

  type Encode[A]       = Kleisli[BindExpression, A, A]
  type Constant[A]     = Kleisli[LiteralExpression, A, A]
  type Decode[F[_], A] = Kleisli[F, DQLNode[A], Vector[A]]

  object syntax extends ModelSyntax {
    implicit class WriteOps[A: Encode](value: A) {
      def param: Expression[A] = implicitly[Encode[A]].run(value)
    }
    implicit class LiteralOps[A: Constant](value: A) {
      def literal: Expression[A] = implicitly[Constant[A]].run(value)
    }
    implicit class BackendEffectDQLOps[F[_]: Jam, A](n: DQLNode[A]) {
      def query(implicit r: Decode[F, A], ns: NamingStrategy): F[Vector[A]] =
        Jam[F].query(n)
    }
    implicit class BackendEffectDQLConversionOps[F[_]: Jam: Monad, L <: HList](n: DQLNode[L]) {
      import cats.implicits._
      def to[A](implicit g: Generic.Aux[A, L], r: Decode[F, L], ns: NamingStrategy): F[Vector[A]] =
        Jam[F].query(n).map(ls => ls.map(g.from))
    }
    implicit class BackendEffectDMLOps[F[_]: Jam, A](n: DMLNode[A]) {
      def update(implicit ns: NamingStrategy): F[Int] = Jam[F].update(n)
    }
  }

}
