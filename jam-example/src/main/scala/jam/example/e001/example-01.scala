package jam.example.e001

import jam.example.model._
import jam.sql._
import shapeless._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Queries {
  import jam.sql.syntax._

  val e: CountryEntity.type = CountryEntity

  def q01(name: Name, limit: Long)(
      implicit et: Encode[Name],
      el: Constant[Long]): DQLNode[CountryCode :: Name :: HNil] =
    DQL
      .from(e)
      .where(e.name like name.param)
      .limit(limit.literal)
      .select(e.code :: e.name)

}

object SlickExample {
  import jam.slick.implicits._
  import jam.slick.jdbcProfile.api._
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  Class.forName("org.postgresql.Driver")

  val db: Database = Database.forURL(
    url = "jdbc:postgresql:demo",
    user = "jeelona",
    password = "jeelona"
  )

  case class SomeModel(code: CountryCode, name: Name)

  def main(args: Array[String]): Unit = {
    val f: Future[Option[SomeModel]] =
      Queries
        .q01(Name("%a%"), 1L)
        .to[SomeModel]
        .map(_.headOption)
        .transactionally
        .unsafeToFuture(db)

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)
    db.close()
  }

}

object DoobieExample {
  import cats.effect.IO
  import doobie._
  import doobie.implicits._
  import jam.doobie.implicits._
  import scala.concurrent.ExecutionContext.Implicits.global

  val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:demo",
    "jeelona",
    "jeelona"
  )

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  def main(args: Array[String]): Unit = {
    val f: Future[Option[(CountryCode, Name)]] =
      Queries
        .q01(Name("%a%"), 1L)
        .to[(CountryCode, Name)]
        .map(_.headOption)
        .transact(xa)
        .unsafeToFuture()

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)
    ()
  }

}
