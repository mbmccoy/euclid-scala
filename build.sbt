lazy val root = project
  .in(file("."))
  .settings(
    name := "euclid-scala",
    version := "0.1",
    scalaVersion := "2.13.6",
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "algebra" % "2.2.3"
    )
)
