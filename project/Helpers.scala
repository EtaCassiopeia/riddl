import sbt.Keys.{name, packageDoc, publishArtifact, resourceGenerators, sourceDirectory, target}
import sbt._
import sbt.io.Path.allSubpaths
import scoverage.ScoverageKeys.{coverageEnabled, coverageFailOnMinimum, coverageMinimumBranchTotal, coverageMinimumStmtTotal}

/** V - Dependency Versions object */
object V {
  val cats = "2.7.0"
  val config = "1.4.1"
  val fastparse = "2.3.3"
  val pureconfig = "0.17.1"
  val scalacheck = "1.15.4"
  val scalatest = "3.2.9"
  val scopt = "4.0.1"
  val ujson = "1.5.0"
}

object Dep {
  val cats_core = "org.typelevel" %% "cats-core" % V.cats
  val config = "com.typesafe" % "config" % V.config
  val fastparse = "com.lihaoyi" %% "fastparse" % V.fastparse
  val pureconfig = "com.github.pureconfig" %% "pureconfig" % V.pureconfig
  val scalactic = "org.scalactic" %% "scalactic" % V.scalatest % "test"
  val scalatest = "org.scalatest" %% "scalatest" % V.scalatest % "test"
  val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test"
  val scopt = "com.github.scopt" %% "scopt" % V.scopt
  val ujson = "com.lihaoyi" %% "ujson" % V.ujson

  val testing = Seq(scalactic, scalatest, scalacheck)

}

object C {
  def withCoverage(p: Project): Project = {
    p.settings(
      coverageEnabled := true,
      coverageFailOnMinimum := true,
      coverageMinimumStmtTotal := 80,
      coverageMinimumBranchTotal := 80
    )
  }

  private def makeThemeResource(name: String, from: File, targetDir: File): Seq[File] = {
    val zip = name + ".zip"
    val distTarget = targetDir / zip
    IO.copyDirectory(from, targetDir)
    val dirToZip = targetDir / name
    IO.zip(allSubpaths(dirToZip), distTarget, None)
    Seq(distTarget)
  }


  def zipResource(srcDir: String) (p :Project): Project = {
    p.settings(
      Compile / resourceGenerators += Def.task {
        val projectName = name.value
        val from = sourceDirectory.value / srcDir
        val targetDir = target.value / "dist"
        makeThemeResource(projectName, from, targetDir)
      }.taskValue,
      Compile / packageDoc / publishArtifact := false,
    )
  }
}
