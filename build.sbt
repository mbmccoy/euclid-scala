lazy val root = project
  .in(file("."))
  .settings(
    name := "euclid-scala",
    version := "0.1",
    scalaVersion := "3.0.2",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.10",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test",
      "org.scalatest" %% "scalatest-flatspec" % "3.2.10" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "algebra" % "2.2.3",
      "com.fasterxml.jackson.module" % "jackson-module-scala_3" % "2.13.0",
      "org.json4s" %% "json4s-jackson" % "4.0.3"

    )
)
