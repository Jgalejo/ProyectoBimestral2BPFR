ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "PReactiva2B",
    libraryDependencies += "com.lihaoyi" %% "scalarx" % "0.4.3",
    libraryDependencies += "io.reactivex" %% "rxscala" % "0.27.0"

  )


