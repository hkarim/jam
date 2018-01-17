package jam.sql

trait Jam[F[_]] {
  def query[A](n: DQLNode[A])(implicit r: Decode[F, A],
                              ns: NamingStrategy): F[Vector[A]]
  def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): F[Int]
}
object Jam {
  def apply[F[_]](implicit ev: Jam[F]): Jam[F] = ev
}
