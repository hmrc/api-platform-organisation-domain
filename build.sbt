import scoverage.ScoverageKeys
import sbt._
import sbt.Keys._
import uk.gov.hmrc.DefaultBuildSettings.targetJvm
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import bloop.integrations.sbt.BloopDefaults

Global / bloopAggregateSourceDependencies := true
Global / bloopExportJarClassifiers := Some(Set("sources"))

val appName = "api-platform-organisation-domain"

//lazy val scala2_13 = "2.13.18"
val scala3 = "3.3.7"

ThisBuild / majorVersion     := 0
ThisBuild / isPublicArtefact := true
ThisBuild / scalaVersion     := scala3

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
lazy val commonSettings = Seq(

  scalafixConfig := {
    val base = (ThisBuild / baseDirectory).value
    val file =
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => base / ".scalafix-scala3.conf"
        case _            => base / ".scalafix-scala2.conf"
      }
    Some(file)
  },

  scalafmtConfig := {
    val base = (ThisBuild / baseDirectory).value
    val file =
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => base / ".scalafmt-scala3.conf"
        case _            => base / ".scalafmt-scala2.conf"
      }
    file
  }

//  scalacOptions ++=
//    (CrossVersion.partialVersion(scalaVersion.value) match {
//      case Some((3, _)) => scala3Options
//      case _            => scala2Options
//    }),

//  crossScalaVersions := Seq(scala3, scala2_13),
)

lazy val library = (project in file("."))
  .settings(
    commonSettings,
    publish / skip := true
  )
  .aggregate(
    apiPlatformApplicationDomain, apiPlatformApplicationDomainFixtures, apiPlatformApplicationDomainTest
  )


lazy val apiPlatformApplicationDomain = Project("api-platform-organisation-domain", file("api-platform-organisation-domain"))
  .settings(
    commonSettings,
    libraryDependencies ++= LibraryDependencies.applicationDomain,
    ScoverageSettings(),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-eT"),
  )
  .disablePlugins(JUnitXmlReportPlugin)


lazy val apiPlatformApplicationDomainFixtures = Project("api-platform-organisation-domain-fixtures", file("api-platform-organisation-domain-fixtures"))
  .dependsOn(
    apiPlatformApplicationDomain % "compile"
  )
  .settings(
    commonSettings,
    libraryDependencies ++= LibraryDependencies.root,
    ScoverageKeys.coverageEnabled := false,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-eT"),
  )
  .disablePlugins(JUnitXmlReportPlugin)


lazy val apiPlatformApplicationDomainTest = Project("api-platform-organisation-domain-test", file("api-platform-organisation-domain-test"))
  .dependsOn(
    apiPlatformApplicationDomain,
    apiPlatformApplicationDomainFixtures
  )
  .settings(
    commonSettings,
    publish / skip := true,
    libraryDependencies ++= LibraryDependencies.root,
    ScoverageSettings(),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-eT"),
  )
  .disablePlugins(JUnitXmlReportPlugin)


  commands ++= Seq(
    Command.command("run-all-tests") { state => "test" :: state },
    Command.command("clean-and-test") { state => "clean" :: "run-all-tests" :: state },
    Command.command("pre-commit") { state => "clean" :: "scalafmtAll" :: "scalafixAll" ::"coverage" :: "run-all-tests" :: "coverageOff" :: "coverageAggregate" :: state }
  )
