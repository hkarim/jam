package jam.example.e004

import jam.example.model._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object SlickOptionals {

  import jam.slick.implicits._
  import jam.slick.jdbcProfile.api._
  import jam.sql._
  import scala.concurrent.ExecutionContext.Implicits.global

  Class.forName("org.postgresql.Driver")

  val db: Database = Database.forURL(
    url = "jdbc:postgresql:demo",
    user = "jeelona",
    password = "jeelona"
  )
  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  val c: CountryEntity.type = CountryEntity

  def main(args: Array[String]): Unit = {

    val c = CountryEntity

    val country = Country(
      code = CountryCode("CDD"),
      name = Name("name"),
      location = None,
      surfaceArea = 0d,
      independenceYear = Some(0),
      population = Population(0L),
      lifeExpectancy = 0f
    )

    val f =
      DML
        .insertInto(c)
        .values(country.param)
        .update
        .transactionally
        .unsafeToFuture(db)

    /*
    val f =
      DQL
        .from(c)
        .limit(10L.literal)
        .select(c)
        .query
        .unsafeToFuture(db)
    */

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)

    ()

  }

}


object DoobieOptionals {

  import cats.effect.IO
  import doobie._
  import doobie.implicits._
  import jam.doobie.implicits._
  import jam.sql._
  import scala.concurrent.ExecutionContext.Implicits.global



  val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:demo",
    "jeelona",
    "jeelona"
  )

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  val c: CountryEntity.type = CountryEntity

  def main(args: Array[String]): Unit = {

    val c = CountryEntity

    /*
    val country = Country(
      code = CountryCode("CCD"),
      name = Name("name"),
      location = None,
      surfaceArea = 0d,
      independenceYear = Some(0),
      population = Population(0L),
      lifeExpectancy = 0f
    )
    */

    val f =
      DQL
        .from(c)
        .limit(10L.literal)
        .select(c)
        .query
        .transact(xa)
        .unsafeToFuture()

    /*
    val f =
      DML
        .insertInto(c)
        .values(country.param)
        .update
        .transact(xa)
        .unsafeToFuture()
    */

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)

    ()

  }
}
