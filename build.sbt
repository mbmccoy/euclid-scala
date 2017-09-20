val dottyVersion = "0.3.0-RC2"

lazy val root = (project in file(".")).
  settings(
    name := "euclid-scala",
    version := "0.1",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
