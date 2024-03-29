import sbt._
import Keys._
import Versions._
import _root_.sbt.librarymanagement.Http

object Dependencies {

  def derevo(artifact: String): ModuleID    = "tf.tofu"           %% s"derevo-$artifact"    % derevoVersion
  def circe(artifact: String): ModuleID     = "io.circe"          %% s"circe-$artifact"     % circeVersion
  def http4s(artifact: String): ModuleID    = "org.http4s"        %% s"http4s-$artifact"    % http4sVersion
  def cormorant(artifact: String): ModuleID = "io.chrisdavenport" %% s"cormorant-$artifact" % cormorantVersion

  object Misc {
    val newtype = "io.estatico"   %% "newtype"  % newtypeVersion
    val squants = "org.typelevel" %% "squants"  % squantsVersion
    val fs2Core = "co.fs2"        %% "fs2-core" % fs2Version
    val fs2IO   = "co.fs2"        %% "fs2-io"   % fs2Version
  }

  object Refined {
    val refinedCore      = "eu.timepit" %% "refined"           % refinedVersion
    val refinedCats      = "eu.timepit" %% "refined-cats"      % refinedVersion
    val refinedShapeless = "eu.timepit" %% "refined-shapeless" % refinedVersion
  }

  object Circe {
    val circeCore    = circe("core")
    val circeGeneric = circe("generic")
    val circeParser  = circe("parser")
    val circeRefined = circe("refined")
  }

  object Derevo {
    val derevoCore          = derevo("core")
    val derevoCats          = derevo("cats")
    val derevoCiris         = derevo("ciris")
    val derevoCirce         = derevo("circe")
    val derevoCirceMagnolia = derevo("circe-magnolia")
  }

  object Cormorant {
    val core    = cormorant("core")
    val generic = cormorant("generic")
    val parser  = cormorant("parser")
    val refined = cormorant("refined")
    val fs2     = cormorant("fs2")
  }

  object Cats {
    val cats       = "org.typelevel" %% "cats-core"   % catsVersion
    val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  }

  object Skunk {
    val skunkCore  = "org.tpolecat" %% "skunk-core"  % skunkVersion
    val skunkCirce = "org.tpolecat" %% "skunk-circe" % skunkVersion
  }

  object Ciris {
    val cirisCore    = "is.cir" %% "ciris"            % cirisVersion
    val cirisEnum    = "is.cir" %% "ciris-enumeratum" % cirisVersion
    val cirisRefined = "is.cir" %% "ciris-refined"    % cirisVersion
    val cirisCirce   = "is.cir" %% "ciris-circe"      % cirisVersion
    val cirisSquants = "is.cir" %% "ciris-squants"    % cirisVersion
  }

  object Http4s {
    val http4sDsl    = http4s("dsl")
    val http4sServer = http4s("ember-server")
    val http4sClient = http4s("ember-client")
    val http4sCirce  = http4s("circe")
  }

  val http4sJwtAuth = "dev.profunktor" %% "http4s-jwt-auth" % http4sJwtAuthVersion

  val monocleCore = "dev.optics"      %% "monocle-core" % monocleVersion
  val javaxCrypto = "javax.xml.crypto" % "jsr105-api"   % javaxCryptoVersion

  val flywayDb = "org.flywaydb"   % "flyway-core"    % "5.2.4"
  val log4cats = "org.typelevel" %% "log4cats-slf4j" % log4catsVersion

  val redis4catsEffects  = "dev.profunktor" %% "redis4cats-effects"  % redis4catsVersion
  val redis4catsLog4cats = "dev.profunktor" %% "redis4cats-log4cats" % redis4catsVersion

  // Runtime
  val logback = "ch.qos.logback" % "logback-classic" % logbackVersion % Runtime

  object CompilerPlugin {
    val betterMonadicFor = compilerPlugin(
      "com.olegpy" %% "better-monadic-for" % betterMonadicForVersion
    )
    val kindProjector = compilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    )
    val semanticDB = compilerPlugin(
      "org.scalameta" % "semanticdb-scalac" % semanticDBVersion cross CrossVersion.full
    )
  }
  import CompilerPlugin._

  // Scalafix rules
  val organizeImports = "com.github.liancheng" %% "organize-imports" % organizeImportsVersion

  val commonDependencies: Seq[ModuleID] = Seq(Cats.cats, Cats.catsEffect)

  val tradeioDependencies: Seq[ModuleID] =
    commonDependencies ++ Seq(kindProjector, betterMonadicFor, semanticDB) ++
      Seq(Misc.newtype, Misc.squants) ++
      Seq(Derevo.derevoCore, Derevo.derevoCats, Derevo.derevoCiris, Derevo.derevoCirceMagnolia) ++
      Seq(monocleCore) ++
      Seq(Refined.refinedCore, Refined.refinedCats, Refined.refinedShapeless) ++
      Seq(Ciris.cirisCore, Ciris.cirisEnum, Ciris.cirisRefined, Ciris.cirisCirce, Ciris.cirisSquants) ++
      Seq(Cormorant.core, Cormorant.generic, Cormorant.parser, Cormorant.refined) ++
      Seq(Skunk.skunkCore, Skunk.skunkCirce) ++ Seq(log4cats, logback) ++
      Seq(Http4s.http4sServer, Http4s.http4sClient, Http4s.http4sDsl, Http4s.http4sCirce) ++
      Seq(http4sJwtAuth) ++
      Seq(redis4catsEffects, redis4catsLog4cats) ++
      Seq(Circe.circeCore, Circe.circeGeneric, Circe.circeParser, Circe.circeRefined)

  // Test
  val catsLaws          = "org.typelevel"       %% "cats-laws"          % catsVersion
  val log4catsNoOp      = "org.typelevel"       %% "log4cats-noop"      % log4catsVersion
  val monocleLaw        = "dev.optics"          %% "monocle-law"        % monocleVersion
  val refinedScalacheck = "eu.timepit"          %% "refined-scalacheck" % refinedVersion
  val weaverCats        = "com.disneystreaming" %% "weaver-cats"        % weaverVersion
  val weaverDiscipline  = "com.disneystreaming" %% "weaver-discipline"  % weaverVersion
  val weaverScalaCheck  = "com.disneystreaming" %% "weaver-scalacheck"  % weaverVersion

  val testDependencies: Seq[ModuleID] =
    Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      catsLaws,
      log4catsNoOp,
      monocleLaw,
      refinedScalacheck,
      weaverCats,
      weaverDiscipline,
      weaverScalaCheck
    )
}
