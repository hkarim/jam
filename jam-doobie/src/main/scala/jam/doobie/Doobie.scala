package jam.doobie

import _root_.doobie._
import _root_.doobie.syntax.string._
import jam.sql.{Backend, DMLNode, Expression, NamingStrategy}
import shapeless.ProductTypeClass

trait Doobie extends Backend[ConnectionIO, Composite, Param] with AutoDoobie with DoobieSyntax {
  type Fr = Fragment

  def readTypeClass: ProductTypeClass[BackendRead]       = ReadTypeClass
  def writeTypeClass: ProductTypeClass[BackendWrite]     = WriteTypeClass
  def literalTypeClass: ProductTypeClass[BackendLiteral] = LiteralTypeClass

  def query[A: BackendRead](n: Expression[A])(implicit ns: NamingStrategy): ConnectionIO[Vector[A]] =
    run(n).query[A](BackendRead[A].read).to[Vector]

  def update[A](n: DMLNode[A])(implicit ns: NamingStrategy): ConnectionIO[Int] =
    run(n).update.run

  def const(value: String): Fr = doobie.util.fragment.Fragment.const0(value)

  def fragment[A: doobie.Param](value: A): Vector[Fr] = Vector(fr"$value")

}
