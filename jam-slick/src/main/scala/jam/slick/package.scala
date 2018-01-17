package jam

import _root_.slick.jdbc.{ActionBasedSQLInterpolation, JdbcProfile, PositionedParameters, SQLActionBuilder}
import _root_.slick.dbio.DBIO

package object slick {

  type Fragment = SQLActionBuilder

  val jdbcProfile: JdbcProfile = new JdbcProfile {}

  implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation =
    new ActionBasedSQLInterpolation(s)

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
  }

}
