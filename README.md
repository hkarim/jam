# Jam

Jam is an SQL builder with [doobie](https://github.com/tpolecat/doobie) and [slick](https://github.com/slick/slick) backends.
This little library allows you use scala to build *partially typed* SQL expressions.

The code is not currently published, and the library is still in very early stage so you'll have to build from the source for the time being.

## Quick Start

To build queries we need to define some models first:

```scala

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

  def findCountry[F[_]: Jam : Functor](name: String)(implicit w: Write[String], r: Read[F, Country]): F[Option[Country]] =
    DQL
      .from(c)
      .where(c.name === name.param)
      .select(c)
      .query                        // type is auto inferred
      .map(_.headOption)
}
```

A few things to notice here:
- In `CountryEntity`, `properties` shape must match the `Country` case class or we get an error at compile time
- `findCountry` abstracts the doobie `ConnectionIO` and slick `DBIO` type constructors and requires an instance of `Functor` for them
- `findCountry` also requires an evidence that a `String` parameter can be written as a JDBC parameter and that we can read the results of our query as `Country` using the `Read[F, Country]` evidence
- At this point, the query will be built and can be run on both doobie and slick

Here is an example running the query using doobie:

```scala
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

  findCountry[ConnectionIO]("Egypt").transact(xa).unsafeToFuture()
}
```

Or we can use slick if we like:

```scala
def slick: Future[Option[Country]] = {
  import jam.slick.implicits._
  import jam.slick.jdbcProfile.api._

  Class.forName("org.postgresql.Driver")

  val db: Database = Database.forURL(
    url = "jdbc:postgresql:demo",
    user = "jeelona",
    password = "jeelona"
  )

  findCountry[DBIO]("Egypt").unsafeToFuture(db)

}
```

Note that can choose the level of abstraction that we need. For example, `findCountry` could have been defined as:

```scala
def findCountry(name: Expression[String]): DQLNode[Country] =
  DQL.from(c).where(c.name === name).select(c)
```
So, we just build the query, not the backend effect - and we pass the query parameter as raw `Expression`, and the function will return an SQL AST node in this case.
To execute such query, for the doobie case, we can simply do:

```scala
findCountry("Egypt".param).query.map(_.headOption).transact(xa).unsafeRunSync()
```

Or in slick case:

```scala
findCountry("Egypt".param).query.transactionally.map(_.headOption).unsafeToFuture(db)
```

### Auto Derivation

Jam provides auto derivation for slick `GetResult` and `SetParameter` automatically, so you don't have to. So, in the example above, we didn't have to provide a `GetResult` instance for `Country` as we would usually have to. Since, doobie does this by default, we don't have to provide anything for doobie.

In some cases though, we will have to define instances for our models! Jam provides an `Iso` typeclass that helps in this case, and uses instances of `Iso` to derive needed instances for both doobie and slick. For example, to derive read and write instances for `Instant` class:

```scala
import jam.data.Iso

implicit val isoInstantTimestamp: Iso[Instant, Timestamp] =
  Iso.instance[Instant, Timestamp](Timestamp.from)(ts => Instant.ofEpochMilli(ts.getTime))
```

This will make both doobie and slick, read and write instances of `Instant` happily!

### Extending the DSL

Every part of the DSL is an instance of `Expression`, so for example, if we need to support the function `count`, we would do:

```scala
def count[A](e: Expression[A]): FunctionNode[A, Long] = FunctionNode("count", e)
def countCountries: DQLNode[Long] = DQL.from(c).select(count(c.code))
```

Have a look at the `jam-example` project for more complex samples.


### Insert, Update and Delete

Here is the insert syntax of Jam:

```scala
// insert a single instance
DML
  .insertInto(c)
  .values(Country("code", "name", 1L).param)

// bulk insert
DML
  .insertInto(c)
  .values(
    Country("code", "name", 1L).param,
    Country("code", "name", 1L).param
  )

// insert into specific column
DML
  .insertInto(c.of(c.name))
  .values("some-name".param)

// insert into multiple specific columns
DML
  .insertInto(c.of(c.name :: c.population))
  .values("some-name".param :: 1L.param)

// insert into select
DML
  .insertInto(c)
  .subQuery(
    DQL.from(c).select(c)
  )

// types are always validated
DML
  .insertInto(c.of(c.name))
  .subQuery(
    DQL.from(c).select("all-constant".param)  // must match the inserted type
  )
```

Here is the update syntax:

```scala
DML
  .update(c)
  .set(c.name := "some name".param, c.code := "some-code".param)

DML
  .update(c)
  .set(c.name := DQL.select("some name".literal))

DML
  .update(c)
  .set(c.name := DQL.select("some name".literal))
  .where(c.name in ("a".literal, "b".param))  // 'a' will be passed literally, while 'b' will be passed as a parameter
```

and finally the delete syntax:

```scala
DML
  .deleteFrom(c)
  .where(c.population <= 0L.param)
```