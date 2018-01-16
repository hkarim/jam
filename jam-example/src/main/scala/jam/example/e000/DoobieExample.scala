package jam.example.e000

import jam.example.model.{CountryEntity, CountryLanguageEntity, Name}

object DoobieExample {

  import cats.effect.IO
  import doobie._
  import doobie.implicits._
  import jam.doobie.implicits._
  import jam.sql._

  val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:demo",
    "jeelona",
    "jeelona"
  )

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  val c: CountryEntity.type = CountryEntity

  def main(args: Array[String]): Unit = {

    val country         = CountryEntity
    val countryLanguage = CountryLanguageEntity

    DQL
      .from((country as 'c) innerJoin (countryLanguage as 'l) on ('c ~ country.code === 'l ~ countryLanguage.countryCode))
      .where('c ~ country.name === Name("Egypt").param)
      .select('l ~ countryLanguage.language :: 'l ~ countryLanguage.isOfficial)
      .query
      .map(_.headOption)
      .transact(xa)
      .unsafeRunAsync {
        case Left(e)  => e.printStackTrace()
        case Right(v) => println(v)
      }

  }

}
