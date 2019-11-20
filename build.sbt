name := "algebraic-expression-processor"

version := "0.1"

scalaVersion := "2.12.10"

resolvers += Resolver.bintrayRepo("stanch", "maven")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.stanch" %% "reftree" % "1.2.0"
  //  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)