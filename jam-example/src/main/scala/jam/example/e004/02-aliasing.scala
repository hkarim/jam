package jam.example.e004

import jam.example.model._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Aliasing01 {

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

  def main(args: Array[String]): Unit = {

    val l = new CompanyEntity with Aliased[Company] {
      def aliasName: String = "l"
    }

    val r = new CompanyEntity with Aliased[Company] {
      def aliasName: String = "r"
    }


    val f =
      DQL
        .from(l innerJoin r on (l.uuid === r.uuid))
        .select(l.uuid :: r.data.email :: l.data.companyType)
        .query
        .unsafeToFuture(db)

    f.onComplete {
      case Success(v) => println(v)
      case Failure(e) => e.printStackTrace()
    }

    Await.result(f, Duration.Inf)

    ()

  }
}
