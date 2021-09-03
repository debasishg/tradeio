ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "dev.tradex"
ThisBuild / organizationName := "tradex"

ThisBuild / scalafixDependencies += Dependencies.organizeImports

resolvers += Resolver.sonatypeRepo("snapshots")

lazy val root = (project in file("."))
  .settings(
    name := "tradeio"
  )
  .aggregate(core)

lazy val core = (project in file("modules/core")).settings(
  name := "tradeio-core",
  commonSettings,
  // compilerOptions,
  consoleSettings,
  typeSystemEnhancements,
  dependencies
)

lazy val commonSettings = Seq(
  scalafmtOnCompile := true,
  scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info"),
  resolvers += Resolver.sonatypeRepo("snapshots")
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

lazy val dependencies =
  libraryDependencies ++= Dependencies.tradeioDependencies

addCommandAlias("runLinter", ";scalafixAll --rules OrganizeImports")

