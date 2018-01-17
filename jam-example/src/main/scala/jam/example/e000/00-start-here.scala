package jam.example.e000

import jam.sql.{DML, DQL}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import shapeless._

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object Model {

  import jam.sql._         // for entity
  import jam.sql.syntax._  // for sql dsl
  import cats.Functor      // to map on query results
  import cats.implicits._  // for functor instance

  case class Country(code: String, name: String, population: Long)

  object CountryEntity extends Entity[Country] {
    val entityName: String = "country"
    val code: Property[String] = property("code")
    val name: Property[String] = property("name")
    val population: Property[Long] = property("population")
    val properties: Properties[Country] = (code :: name :: population :: HNil).properties[Country]
  }

  val c: CountryEntity.type = CountryEntity

  implicit val ns: NamingStrategy = NamingStrategy.Postgres // or MySQL

  def findCountry[F[_]: Jam : Functor](name: String)
                                      (implicit
                                       ws: Encode[String],
                                       ll: Constant[Long],
                                       r: Decode[F, Country]): F[Option[Country]] =
    DQL
      .from(c)
      .where(c.name === name.param and (c.population - 100L.literal) > 200L.literal)
      .select(c)
      .query
      .map(_.headOption)

  def findCountry2(name: Expression[String]): DQLNode[Country] =
    DQL
      .from(c)
      .where(c.name === name)
      .select(c)

  def count[A](e: Expression[A]): FunctionNode[A, Long] = FunctionNode("count", e)
  def countCountries: DQLNode[Long] = DQL.from(c).select(count(c.code))

}

object Main {

  import Model._

  def doobie: Future[Option[Country]] = {
    import cats.effect.IO
    import _root_.doobie._
    import _root_.doobie.implicits._
    import jam.doobie.implicits._

    val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:demo",
      "jeelona",
      "jeelona"
    )

    findCountry2("Egypt".param).query.map(_.headOption).transact(xa).unsafeRunSync()



    findCountry[ConnectionIO]("Egypt").transact(xa).unsafeToFuture()
  }

  def slick: Future[Option[Country]] = {
    import jam.slick.implicits._
    import jam.slick.jdbcProfile.api._

    Class.forName("org.postgresql.Driver")

    val db: Database = Database.forURL(
      url = "jdbc:postgresql:demo",
      user = "jeelona",
      password = "jeelona"
    )

    DML
      .insertInto(c)
      .values(Country("code", "name", 1L).param)

    DML
      .insertInto(c)
      .values(
        Country("code", "name", 1L).param,
        Country("code", "name", 1L).param
      )

    DML
      .insertInto(c.of(c.name :: c.population))
      .values("some-name".param :: 1L.param)

    DML
      .insertInto(c.of(c.name))
      .subQuery(
        DQL.from(c).select("all-constant".param)
      )

    DML
      .update(c)
      .set(c.name := "some name".param, c.code := "some-code".param)

    DML
      .update(c)
      .set(c.name := DQL.select("some name".literal).enclose, c.population := c.population - 1L.literal)
      .where( (c.code :: c.name) in ("a".literal :: "b".param))
      .update
      .transactionally
      .unsafeToFuture(db)

    DML
      .deleteFrom(c)
      .where(c.population <= 0L.param)

    c.population + 1L.literal
    c.population - 1L.literal
    c.population * 1L.literal
    c.population / 1L.literal
    //c.name - "".literal

    c.population > 1L.literal

    findCountry[DBIO]("Egypt").unsafeToFuture(db)

  }

  def main(args: Array[String]): Unit = {
    val f = for {
      dc <- doobie
      sc <- slick
    } yield (dc, sc)

    f.onComplete {
      case Success((l, r)) =>
        println(s"doobie: $l")
        println(s"slick: $r")
      case Failure(e) =>
        e.printStackTrace()
    }

    Await.result(f, Duration.Inf)

    println("All Done!")

  }
}
