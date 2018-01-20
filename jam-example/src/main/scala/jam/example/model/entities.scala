package jam.example.model

import java.time.Instant

import jam.sql.{Composite, Entity, Properties, Property}
import jam.sql.syntax._
import shapeless._

class SignatureComposite(atName: String, byName: String) extends Composite[Signature] {
  val at: Property[Instant]  = property(atName)
  val by: Property[UserUUID] = property(byName)
  val properties: Properties[Signature] =
    (at :: by :: HNil).properties[Signature]
}

class TraceComposite extends Composite[Trace] {
  object created extends SignatureComposite("created_at", "created_by")
  object updated extends SignatureComposite("updated_at", "updated_by")
  val properties: Properties[Trace] =
    (created.widen :: updated.widen :: HNil).properties[Trace]
}

class CompanyEntity extends Entity[Company] {
  val entityName: String          = "company"
  val uuid: Property[CompanyUUID] = property("uuid")
  object data extends Composite[CompanyData] {
    val name: Property[Name]               = property("name")
    val email: Property[Option[Email]]     = property("email")
    val companyType: Property[CompanyType] = property("company_type")
    val properties: Properties[CompanyData] =
      (name :: email :: companyType :: HNil).properties[CompanyData]
  }
  object trace extends TraceComposite
  val properties: Properties[Company] =
    (uuid :: data.widen :: trace.widen :: HNil).properties[Company]
}
object CompanyEntity extends CompanyEntity

object CityEntity extends Entity[City] {
  val entityName: String                 = "city"
  val id: Property[CityId]               = property("id")
  val name: Property[Name]               = property("name")
  val countryCode: Property[CountryCode] = property("countrycode")
  val district: Property[String]         = property("district")
  val population: Property[Population]   = property("population")
  val properties: Properties[City] =
    (id :: name :: countryCode :: district :: population :: HNil)
      .properties[City]
}

object CountryEntity extends Entity[Country] {
  val entityName: String          = "country"
  val code: Property[CountryCode] = property("code")
  val name: Property[Name]        = property("name")
  object location extends Composite[Location] {
    val continent: Property[String] = property("continent")
    val region: Property[Option[String]]    = property("region")

    val properties: Properties[Location] = (continent :: region :: HNil).properties[Location]
  }
  val surfaceArea: Property[Double]           = property("surfacearea")
  val independenceYear: Property[Option[Int]] = property("indepyear")
  val population: Property[Population]        = property("population")
  val lifeExpectancy: Property[Float]         = property("lifeexpectancy")
  val properties: Properties[Country] =
    (code :: name :: location.optional :: surfaceArea :: independenceYear :: population :: lifeExpectancy :: HNil)
      .properties[Country]
}

object CountryLanguageEntity extends Entity[CountryLanguage] {
  val entityName: String                 = "countrylanguage"
  val countryCode: Property[CountryCode] = property("countrycode")
  val language: Property[String]         = property("language")
  val isOfficial: Property[Boolean]      = property("isofficial")
  val percentage: Property[Float]        = property("percentage")
  val properties: Properties[CountryLanguage] =
    (countryCode :: language :: isOfficial :: percentage :: HNil)
      .properties[CountryLanguage]

}
