import laika.helium.config.ThemeLink
import laika.theme.config.Color
import laika.helium.Helium
import com.typesafe.tools.mima.core._
import xerial.sbt.Sonatype._
import org.typelevel.sbt.TypelevelMimaPlugin

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "io.github.arainko"
ThisBuild / organizationName := "arainko"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("arainko", "Aleksander Rainko"))
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / tlSitePublishBranch := Some("master")

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[Problem]("io.github.arainko.ducktape.internal.*")
)

ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / tlCiScalafixCheck := true
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowUseSbtThinClient := true
ThisBuild / githubWorkflowBuild += WorkflowStep.Run(
  name = Some("Check docs"),
  commands = "sbt --client docs/mdoc" :: Nil,
  cond = Some(s"matrix.project == '${root.jvm.id}'")
)

ThisBuild / tlVersionIntroduced := Map("3" -> "0.1.6")

lazy val root = tlCrossRootProject.aggregate(ducktape)

lazy val ducktape =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .enablePlugins(TypelevelMimaPlugin)
    .in(file("ducktape"))
    .settings(
      scalacOptions ++= List("-Xcheck-macros", "-no-indent", "-old-syntax", "-Xfatal-warnings", "-deprecation"),
      libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M7" % Test
    )
    .jvmSettings(tlMimaPreviousVersions ++= Set("0.1.0", "0.1.1", "0.1.2", "0.1.3", "0.1.4", "0.1.5"))

lazy val docs =
  project
    .in(file("documentation"))
    .enablePlugins(TypelevelSitePlugin)
    .settings(
      tlSiteHeliumConfig := {
        val current = tlSiteHeliumConfig.value
        modifyTheme(current)
      },
      libraryDependencies += ("org.scalameta" %% "scalafmt-dynamic" % "3.6.1").cross(CrossVersion.for3Use2_13)
    )
    .dependsOn(ducktape.jvm)

def modifyTheme(helium: Helium) = {
  helium.site
    .themeColors(
      primary = Color.rgb(177, 178, 255),
      primaryMedium = Color.rgb(170, 196, 255),
      primaryLight = Color.rgb(238, 241, 255),
      secondary = Color.rgb(210, 218, 255),
      text = Color.hex("FFF"),
      background = Color.hex("394867"),
      bgGradient = Color.hex("394867") -> Color.hex("394867")
    )
    .site
    .favIcons()
    .site
}
