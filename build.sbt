import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.2",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.1",
  "no.arktekk" %% "anti-xml" % "0.5.1",
  "org.reactormonk" %% "counter" % "1.2.0",
  "edu.cmu.cs" % "ark-tweet-nlp" % "0.3.2"
)