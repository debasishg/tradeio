lazy val root = (project in file(".")).settings(
  commonSettings,
  compilerOptions,
  consoleSettings,
  typeSystemEnhancements,
  dependencies,
  tests
)

lazy val commonSettings = Seq(
  organization := "org.github",
  name := "tradeio",
  scalaVersion := Versions.scalaVersion,
  scalafmtOnCompile := true
)

lazy val consoleSettings = Seq(
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import"
)

lazy val compilerOptions = {
  val commonOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8",
    "-target:jvm-1.8",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Ywarn-value-discard",
    "-Ymacro-annotations",
    "-Ywarn-unused:imports"
  )

  scalacOptions ++= commonOptions 
}

lazy val typeSystemEnhancements = 
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

def dep(org: String)(version: String)(modules: String*) =
  Seq(modules: _*) map { name =>
    org %% name % version
  }

lazy val dependencies =
  libraryDependencies ++= Dependencies.tradeioDependencies

lazy val tests = {
  val dependencies =
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.2",
      "org.scalatest" %% "scalatest" % "3.0.8",
      "org.typelevel" %% "cats-effect-laws" % "2.0.0"
    ).map(_ % "test")

  val frameworks =
    testFrameworks := Seq(TestFrameworks.ScalaTest)

  Seq(dependencies, frameworks)
}
