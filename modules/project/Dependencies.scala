import sbt._
import Keys._
import Versions._

object Dependencies {

  def derevo(artifact: String): ModuleID = "tf.tofu"       %% s"derevo-$artifact"                % derevoVersion

  object Misc {
    val newtype           = "io.estatico"                  %% "newtype"                          % newtypeVersion
    val squants           = "org.typelevel"                %% "squants"                          % squantsVersion
    val fs2Core           = "co.fs2"                       %% "fs2-core"                         % fs2Version
    val fs2IO             = "co.fs2"                       %% "fs2-io"                           % fs2Version
  }

  object Refined {
    val refinedCore       = "eu.timepit"                   %% "refined"                          % refinedVersion
    val refinedCats       = "eu.timepit"                   %% "refined-cats"                     % refinedVersion
    val refinedShapeless  = "eu.timepit"                   %% "refined-shapeless"                % refinedVersion
  }

  object Derevo {
    val derevoCore  = derevo("core")
    val derevoCats  = derevo("cats")
    val derevoCirce = derevo("circe-magnolia")
  }

  object Cormorant {
    val core              = "io.chrisdavenport" %% "cormorant-core"     % cormorantVersion
    val generic           = "io.chrisdavenport" %% "cormorant-generic"  % cormorantVersion
    val parser            = "io.chrisdavenport" %% "cormorant-parser"   % cormorantVersion
    val refined           = "io.chrisdavenport" %% "cormorant-refined"  % cormorantVersion
    val fs2               = "io.chrisdavenport" %% "cormorant-fs2"      % cormorantVersion
  }

  object Cats {
    val cats              = "org.typelevel"                %%   "cats-core"                      % catsVersion
    val catsEffect        = "org.typelevel"                %%   "cats-effect"                    % catsEffectVersion
  }

  object Skunk {
    val skunkCore         = "org.tpolecat"                 %% "skunk-core"                       % skunkVersion
    val skunkCirce        = "org.tpolecat"                 %% "skunk-circe"                      % skunkVersion
  }

  object Ciris {
    val cirisCore         = "is.cir"                       %% "ciris"                            % cirisVersion
    val cirisEnum         = "is.cir"                       %% "ciris-enumeratum"                 % cirisVersion
    val cirisRefined      = "is.cir"                       %% "ciris-refined"                    % cirisVersion
  }

  val monocleCore         = "dev.optics"                   %% "monocle-core"                     % monocleVersion

  val flywayDb            = "org.flywaydb"                  % "flyway-core"                      % "5.2.4"
  val log4cats            = "org.typelevel"                %% "log4cats-slf4j"                   % log4catsVersion

  // Runtime
  val logback             = "ch.qos.logback"                % "logback-classic"                  % logbackVersion % Runtime

  val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.12.0" cross CrossVersion.full)

  val commonDependencies: Seq[ModuleID] = Seq(Cats.cats, Cats.catsEffect)

  val tradeioDependencies: Seq[ModuleID] = 
    commonDependencies ++ Seq(kindProjector) ++ 
      Seq(Misc.newtype, Misc.squants) ++ 
      Seq(Derevo.derevoCore, Derevo.derevoCats, Derevo.derevoCirce) ++
      Seq(monocleCore) ++
      Seq(Refined.refinedCore, Refined.refinedCats, Refined.refinedShapeless) ++ 
      Seq(Ciris.cirisCore, Ciris.cirisEnum, Ciris.cirisRefined) ++ 
      Seq(Cormorant.core, Cormorant.generic, Cormorant.parser, Cormorant.refined) ++
      Seq(Skunk.skunkCore, Skunk.skunkCirce) ++ Seq(log4cats, logback)
}
