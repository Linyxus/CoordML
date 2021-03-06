name := "CoordML"

version := "0.1"

scalaVersion := "2.13.4"

lazy val akkaHttpVersion = "10.2.1"
lazy val akkaVersion = "2.6.10"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "ch.megard" %% "akka-http-cors" % "1.1.0",
  "com.github.julien-truffaut" %% "monocle-core"  % "2.0.3",
  "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8",

  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
)

scalacOptions in Global += "-Ymacro-annotations"

enablePlugins(JavaAppPackaging)

//mainClass in Compile := Some("central.CentralApp")
//
//discoveredMainClasses in Compile := Seq()
