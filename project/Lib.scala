import sbt._

object Lib {

  object Version {
    val logbackClassic         = "1.2.3"
    val slf4j                  = "1.6.4"
    val scalaLogging           = "3.5.0"
    val shapeless              = "2.3.3"
    val cats                   = "1.0.1"
    val circe                  = "0.9.0"
    val postgres               = "42.1.4"
    val mysql                  = "6.0.6"
    val slick                  = "3.2.1"
    val doobie                 = "0.5.0-M13"
    val akkaActor              = "2.5.8"
    val akkaStream             = "2.5.8"
    val akkaHttp               = "10.0.11"
    val akkaHttpCirce          = "1.19.0"
    val spire                  = "0.14.1"
    val jbcrypt                = "0.4"
    val javaJWT                = "3.2.0"
    val scalaTest              = "3.0.1"
    val akkaHttpTestkit        = "10.0.7"
  }

  val logback = Seq(
    "ch.qos.logback" % "logback-classic" % Version.logbackClassic
  )

  val sfl4j = Seq(
    "org.slf4j" % "slf4j-nop" % Version.slf4j
  )

  val scalaLogging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % Version.scalaLogging
  )

  val cats = Seq(
    "org.typelevel" %% "cats-core" % Version.cats
  )

  val circe = Seq(
    "io.circe" %% "circe-core"    % Version.circe,
    "io.circe" %% "circe-generic" % Version.circe,
    "io.circe" %% "circe-parser"  % Version.circe
  )

  val shapeless = Seq(
    "com.chuusai" %% "shapeless" % Version.shapeless
  )

  val postgres = Seq(
    "org.postgresql" % "postgresql" % Version.postgres
  )

  val mysql = Seq(
    "mysql" % "mysql-connector-java" % Version.mysql
  )

  val slick = Seq(
    "com.typesafe.slick" %% "slick"          % Version.slick,
    "com.typesafe.slick" %% "slick-hikaricp" % Version.slick
  )

  val doobie = Seq(
    "org.tpolecat" %% "doobie-core"      % Version.doobie
  )

  val doobieHikari = Seq(
    "org.tpolecat" %% "doobie-hikari"      % Version.doobie
  )

  val jbcrypt = Seq(
    "org.mindrot" % "jbcrypt" % Version.jbcrypt
  )

  val akkaActor = Seq(
    "com.typesafe.akka" %% "akka-actor" % Version.akkaActor
  )

  val akkaStream = Seq(
    "com.typesafe.akka" %% "akka-stream" % Version.akkaStream
  )

  val akkaHttp = Seq(
    "com.typesafe.akka" %% "akka-http" % Version.akkaHttp
  )

  val akkaHttpCirce = Seq(
    "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce
  )

  val spire = Seq(
    "org.typelevel" %% "spire" % Version.spire
  )

  val javaJWT = Seq(
    "com.auth0" % "java-jwt" % Version.javaJWT
  )

  val scalaTest = Seq(
    "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
  )

  val akkaHttpTestkit = Seq(
    "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttpTestkit % "test"
  )
}
