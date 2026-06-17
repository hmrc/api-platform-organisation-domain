import sbt._

object LibraryDependencies {
  def apply() = compileDependencies ++ testDependencies
  
  val commonDomainVersion = "1.1.0"
  val monocleVersion = "3.1.0"

  val compileDependencies = Seq(
    ("uk.gov.hmrc"             %% "api-platform-common-domain"     % commonDomainVersion),
//    ("com.github.t3hnar"       %% "scala-bcrypt"                   % "4.3.0").cross(CrossVersion.for3Use2_13),
    "de.svenkubiak"            % "jBCrypt"      % "0.4.3",
    "com.typesafe"             % "config"                         % "1.4.2",
    "commons-validator"        % "commons-validator"              % "1.10.1",
    "dev.optics"              %% "monocle-core"                   % monocleVersion,
    "dev.optics"              %% "monocle-macro"                  % monocleVersion
  )

  val testDependencies = Seq(
    "org.scalactic"           %% "scalactic"                            % "3.2.14",
    "com.vladsch.flexmark"     % "flexmark-all"                         % "0.64.8",
    "org.mockito"             %% "mockito-scala-scalatest"              % "2.2.1",
    "org.scalatest"           %% "scalatest"                            % "3.2.19",
    ("uk.gov.hmrc"             %% "api-platform-common-domain-fixtures"  % commonDomainVersion)
  )

  val applicationDomain = compileDependencies ++ testDependencies.map(_ % "test")

  val root = compileDependencies ++ testDependencies
  
}
