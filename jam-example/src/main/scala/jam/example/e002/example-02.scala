package jam.example.e002

import java.util.UUID

import cats.Monad
import jam.example.model._
import jam.sql._

trait CountryService[F[_]] {
  def find(code: CountryCode)(implicit w: Encode[CountryCode], r: Decode[F, Country]): F[Option[Country]]
}

class DBCountryService[F[_]: Jam: Monad](implicit val ns: NamingStrategy) extends CountryService[F] {
  import jam.sql.syntax._
  import cats.implicits._

  val e: CountryEntity.type = CountryEntity

  def find(code: CountryCode)(implicit w: Encode[CountryCode], r: Decode[F, Country]): F[Option[Country]] =
    DQL.from(e).where(e.code === code.param).select(e).query.map(_.headOption)

}

object SlickExample {
  import jam.slick.implicits._
  import jam.slick.jdbcProfile.api._
  import scala.concurrent.ExecutionContext.Implicits.global

  Class.forName("org.postgresql.Driver")

  val db: Database = Database.forURL(
    url = "jdbc:postgresql:demo",
    user = "jeelona",
    password = "jeelona"
  )

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  val companyService: CountryService[DBIO] = new DBCountryService[DBIO]

  companyService.find(CountryCode("")).transactionally
}

object DoobieExample {
  import cats.effect.IO
  import doobie._
  import doobie.implicits._
  import jam.doobie.implicits._

  val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:demo",
    "jeelona",
    "jeelona"
  )

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  implicit val metaUUID: Meta[UUID]               = isoMeta[UUID, String]
  implicit val metaCompanyType: Meta[CompanyType] = isoMeta[CompanyType, String]

  val companyService: CountryService[ConnectionIO] = new DBCountryService[ConnectionIO]
  companyService.find(CountryCode("")).transact(xa)

}
