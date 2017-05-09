name := "practice-fun"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.1" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "com.storm-enroute" % "scalameter_2.11" % "0.8.2"
)
