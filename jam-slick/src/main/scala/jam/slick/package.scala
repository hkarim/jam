package jam

import _root_.slick.dbio.DBIO
import _root_.slick.jdbc.{JdbcProfile, PositionedParameters, SQLActionBuilder}

package object slick {

  type Fragment = SQLActionBuilder

  val jdbcProfile: JdbcProfile = new JdbcProfile {}

  /*
  implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation =
    new ActionBasedSQLInterpolation(s)
  */

  def concat(a: Fragment, b: Fragment): Fragment =
    SQLActionBuilder(a.queryParts ++ b.queryParts, (p: Unit, pp: PositionedParameters) => {
      a.unitPConv.apply(p, pp)
      b.unitPConv.apply(p, pp)
    })

  object implicits extends Slick {
    implicit class DBIOActionOps[A](dbio: DBIO[A]) {
      import scala.concurrent.Future
      def unsafeToFuture(db: jdbcProfile.api.Database): Future[A] =
        db.run(dbio)
    }

    //implicit def gr[A](implicit r: Lazy[Read[A]]): GetResult[A] = r.value.read
    //implicit def sp[A](implicit w: Lazy[Write[A]]): SetParameter[A] = w.value.write
  }

}
