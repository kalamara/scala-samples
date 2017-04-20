name := "FunctionalProgrammingInScalaBeagle"

version := "1.0"
scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

scalaVersion := "2.11.8"
libraryDependencies ++= {
  val scalaTestV = "2.2.6"
  Seq(
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  )
}