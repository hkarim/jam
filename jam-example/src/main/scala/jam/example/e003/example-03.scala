package jam.example.e003

import java.util.UUID

import jam.example.model._
import jam.sql._
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.ExecutionContext.Implicits.global

trait CompanyService[F[_]] {
  def find(uuid: CompanyUUID): F[Option[Company]]
  def save(instance: Company): F[Int]
  def update(uuid: CompanyUUID, data: CompanyData): F[Int]
}

import jam.slick.implicits._

class SlickCompanyService(implicit val ns: NamingStrategy) extends CompanyService[DBIO] {

  val e: CompanyEntity.type = CompanyEntity

  def find(uuid: CompanyUUID): DBIO[Option[Company]] =
    DQL.from(e).where(e.uuid === uuid.param).select(e).query.map(_.headOption)

  def save(instance: Company): DBIO[Int] =
    DML.insertInto(e).values(instance.param).update

  def update(uuid: CompanyUUID, data: CompanyData): DBIO[Int] =
    DML.update(e).set(e.data := data.param)(e.uuid === uuid.param).update

  DQL
    .from(
      (e as 'l) innerJoin (e as 'r) on ('l ~ e.uuid === 'r ~ e.uuid)
    )
    .where(('l ~ e.data.name) like Name("%some-one%").param)
    .select('l ~ e.uuid :: 'l ~ e.data.email)

}

object SlickExample {

  val db: Database = Database.forURL("jdbc:postgresql:world", "postgres", "")

  implicit val ns: NamingStrategy = NamingStrategy.Postgres

  val companyService: CompanyService[DBIO] = new SlickCompanyService

  companyService.find(CompanyUUID(UUID.randomUUID)).transactionally
}
