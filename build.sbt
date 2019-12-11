name := "algebraic-expression-processor"

version := "0.1"

scalaVersion := "2.12.10"

resolvers += Resolver.bintrayRepo("stanch", "maven")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.stanch" %% "reftree" % "1.2.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
//  "uk.org.lidalia" % "sysout-over-slf4j" % "1.0.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)