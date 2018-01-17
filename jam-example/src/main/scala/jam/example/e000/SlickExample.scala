package jam.example.e000

import jam.example.model.{
  CountryCode,
  CountryEntity,
  CountryLanguageEntity,
  Name
}
import jam.sql._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

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

  val c: CountryEntity.type = CountryEntity

  def main(args: Array[String]): Unit = {

    val country = CountryEntity
    val countryLanguage = CountryLanguageEntity

    DML
      .update(country)
      .set(country.name := Name("").param,
           country.code := CountryCode("").param)
      .where(country.name === Name("").param)

    val query =
      DQL
        .from(
          (country as 'c) innerJoin (countryLanguage as 'l) on ('c ~ country.code === 'l ~ countryLanguage.countryCode))
        .where('c ~ country.name === Name("Egypt").param)
        .select(
          'l ~ countryLanguage.language :: 'l ~ countryLanguage.isOfficial)
        .query
        .map(_.headOption)

    val f = db.run(query)

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)

    db.close()

  }

}
