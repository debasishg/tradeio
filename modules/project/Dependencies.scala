import sbt._
import Keys._
import Versions._

object Dependencies {

  object Misc {
    val newtype           = "io.estatico"                  %% "newtype"                          % newtypeVersion
    val refinedCore       = "eu.timepit"                   %% "refined"                          % refinedVersion
    val refinedCats       = "eu.timepit"                   %% "refined-cats"                     % refinedVersion
    val refinedShapeless  = "eu.timepit"                   %% "refined-shapeless"                % refinedVersion
    val squants           = "org.typelevel"                %% "squants"                          % squantsVersion
  }

  object Cats {
    val cats              = "org.typelevel"                %%   "cats-core"                      % catsVersion
    val catsEffect        = "org.typelevel"                %%   "cats-effect"                    % catsEffectVersion
    val catsMtl           = "org.typelevel"                %%   "cats-mtl-core"                  % catsMtlVersion
  }

  object Doobie {
    val doobieCore        = "org.tpolecat"                 %% "doobie-core"                      % doobieVersion
    val doobieH2          = "org.tpolecat"                 %% "doobie-h2"                        % doobieVersion
    val doobieHikari      = "org.tpolecat"                 %% "doobie-hikari"                    % doobieVersion
    val doobiePostgres    = "org.tpolecat"                 %% "doobie-postgres"                  % doobieVersion
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

  val flywayDb            = "org.flywaydb"                  % "flyway-core"                      % "5.2.4"

  val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

  val commonDependencies: Seq[ModuleID] = Seq(Cats.cats, Cats.catsEffect)

  val tradeioDependencies: Seq[ModuleID] = 
    commonDependencies ++ Seq(Cats.catsMtl) ++ Seq(kindProjector) ++ Seq(Misc.newtype, Misc.refinedCore, Misc.refinedCats, Misc.refinedShapeless, Misc.squants) ++ Seq(Ciris.cirisCore, Ciris.cirisEnum, Ciris.cirisRefined)
}