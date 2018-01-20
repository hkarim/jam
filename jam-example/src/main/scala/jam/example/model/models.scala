package jam.example.model

import java.time.Instant
import java.util.UUID

case class UserUUID(value: UUID) extends AnyVal

case class Signature(at: Instant, by: UserUUID)
case class Trace(created: Signature, updated: Signature)

case class Name(value: String)  extends AnyVal
case class Email(value: String) extends AnyVal

sealed trait CompanyType
object CompanyType extends Enumerator[CompanyType] {
  case object Public        extends CompanyType
  case object PrivatelyHeld extends CompanyType
  def apply(name: String): CompanyType = name match {
    case "PUBLIC"         => Public
    case "PRIVATELY_HELD" => PrivatelyHeld
  }
  def to(value: CompanyType): String = value match {
    case Public        => "PUBLIC"
    case PrivatelyHeld => "PRIVATELY_HELD"
  }
  val enumerate: Vector[CompanyType]                     = Vector(Public, PrivatelyHeld)
  implicit val eCompanyTypeEnum: Enumerator[CompanyType] = CompanyType
}

case class CompanyUUID(value: UUID) extends AnyVal
case class CompanyData(name: Name, email: Option[Email], companyType: CompanyType)
case class Company(uuid: CompanyUUID, data: CompanyData, trace: Trace)

/// world database

case class CityId(value: Long)        extends AnyVal
case class CountryCode(value: String) extends AnyVal
case class Population(value: Long)    extends AnyVal
case class City(id: CityId, name: Name, countryCode: CountryCode, district: String, population: Population)
case class Location(continent: String, region: Option[String])
case class Country(code: CountryCode,
                   name: Name,
                   location: Option[Location],
                   surfaceArea: Double,
                   independenceYear: Option[Int],
                   population: Population,
                   lifeExpectancy: Float)

case class CountryLanguage(countryCode: CountryCode, language: String, isOfficial: Boolean, percentage: Float)
