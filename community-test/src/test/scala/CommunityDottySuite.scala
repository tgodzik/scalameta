package scala.meta.parser.dotty

import scala.meta._
import scala.meta.dialects.Dotty
import munit.FunSuite

import java.io.File

import sys.process._
import scala.language.postfixOps

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path

class CommunityDottySuite extends FunSuite {

  val communityDirectory = Paths.get("community-projects")

  def fetchCommunityBuild(build: CommunityBuild): Unit = {
    if (!Files.exists(communityDirectory)) Files.createDirectory(communityDirectory)

    val folder = communityDirectory.resolve(build.name).toString
    val checkout = build.commit.map(commit => s"&& git checkout $commit ").getOrElse("")
    if (!Files.exists(communityDirectory.resolve(build.name))) {
      val gclone = s"git clone ${build.giturl} ${folder}"
      val gchangecommit = s"""sh -c "cd ${folder} $checkout" """

      println(gchangecommit)
      val result: Int = (gclone #&& gchangecommit) !

      assert(clue(result) == 0, s"Fetching community build ${build.name} failed")
    } else {
      val gchangecommit =
        s"""sh -c "cd ${folder} && git fetch origin $checkout" """
      val result: Int = gchangecommit !

      assert(clue(result) == 0, s"Checking out community build ${build.name} failed")
    }
  }

  case class CommunityBuild(
      giturl: String,
      commit: Option[String],
      name: String,
      excluded: List[String]
  )
  case class TestStats(
      checkedFiles: Int,
      errors: Int,
      lastError: Option[Throwable],
      timeTaken: Long,
      linesParsed: Int
  )

  final val InitTestStats = TestStats(0, 0, None, 0, 0)

  def merger(s1: TestStats, s2: TestStats): TestStats =
    TestStats(
      s1.checkedFiles + s2.checkedFiles,
      s1.errors + s2.errors,
      s1.lastError.orElse(s2.lastError),
      s1.timeTaken + s2.timeTaken,
      s1.linesParsed + s2.linesParsed
    )

  val communityBuilds = List(
    CommunityBuild(
      "https://github.com/lampepfl/dotty.git",
      //commit hash from 01.12.2020
      commit = Some("42a30b8fec95487147fa0d575bfae3cfb9417617"),
      "dotty",
      dottyExclusionList
    ),
    CommunityBuild(
      "https://github.com/scalameta/munit.git",
      // latest commit from 27.11.2020
      commit = Some("5f384b548960ee7731649f5f900eb1d854e7827a"),
      "munit",
      munitExclusionList
    ),
    CommunityBuild("https://github.com/dotty-staging/cats-mtl", None, "cats-mtl", Nil),
    CommunityBuild("https://github.com/dotty-staging/coop", None, "coop", Nil),
    CommunityBuild(
      "https://github.com/dotty-staging/better-files",
      None,
      "betterfiles",
      // extension is used not as keyword
      List("core/src/main/scala/better/files/File.scala")
    ),
    CommunityBuild("https://github.com/dotty-staging/scalacheck", None, "scalacheck", Nil),
  )

  for (build <- communityBuilds) {
    test(s"community-build-${build.name}") {
      check(build)
    }
  }

  def check(implicit build: CommunityBuild): Unit = {
    fetchCommunityBuild(build)

    val stats = checkFilesRecursive(communityDirectory.resolve(build.name))
    val timePer1KLines = stats.timeTaken / (stats.linesParsed / 1000.0)

    println("--------------------------")
    println(s"Files parsed correctly ${stats.checkedFiles}")
    println(s"Files errored: ${stats.errors}")
    println(s"Time taken: ${stats.timeTaken}ms")
    println(s"Lines parsed: ~${stats.linesParsed / 1000}k")
    println(s"Parsing speed per 1k lines ===> ${timePer1KLines} ms/1klines")
    println("--------------------------")
    stats.lastError.foreach(e => throw e)
  }

  def timeIt(block: => Unit): Long = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    t1 - t0
  }

  def checkFilesRecursive(parent: Path)(implicit build: CommunityBuild): TestStats = {
    if (ignoreParts.exists(p => parent.toAbsolutePath.toString.contains(p)))
      return InitTestStats
    if (Files.isDirectory(parent)) {
      import scala.collection.JavaConverters._
      Files
        .list(parent)
        .map(checkFilesRecursive)
        .iterator()
        .asScala
        .fold(InitTestStats)(merger)
    } else {
      if (parent.toAbsolutePath.toString.endsWith(".scala")) {
        checkFile(parent)
      } else InitTestStats
    }
  }

  def checkFile(file: Path)(implicit build: CommunityBuild): TestStats = {
    val fileContent = Input.File(file.toAbsolutePath)
    val lines = fileContent.chars.count(_ == '\n')
    if (excluded(file.toAbsolutePath.toString, build)) {
      try {
        val taken = timeIt {
          fileContent.parse[Source].get
        }
        println("File marked as error but parsed correctly " + file.toAbsolutePath)
        TestStats(1, 1, None, taken, lines)
      } catch {
        case e: Throwable => TestStats(1, 1, None, 0, 0)
      }
    } else {
      try {
        val taken = timeIt {
          fileContent.parse[Source].get
        }
        TestStats(1, 0, None, taken, lines)
      } catch {
        case e: Throwable =>
          println(s"Failed for file ${file.toAbsolutePath}")
          println(s"Error: " + e.getMessage())
          TestStats(1, 1, Some(e), 0, 0)
      }
    }
  }

  def excluded(path: String, build: CommunityBuild): Boolean = {
    build.excluded.exists(el => path.endsWith(el))
  }

  final def dottyExclusionList = List(
    // [scalameta] erased modifier - for now used internally, will be available in 3.1
    "library/src/scala/compiletime/package.scala",
    // most likely will become deprecated: if (cond) <ident>
    "tools/dotc/typer/Implicits.scala",
    "tools/dotc/typer/Checking.scala",
    // if then - else without outdentation before else.
    // it's unlcear what to do in this case
    // https://github.com/lampepfl/dotty/issues/10372
    "dotty/dokka/tasty/ClassLikeSupport.scala",
    // extension will become a keyword, needs fix in dotty
    "dotty/dokka/translators/ScalaSignatureProvider.scala",
    "dotty/tools/io/AbstractFile.scala",
    "dotty/tools/io/JarArchive.scala",
    "dotty/tools/io/Path.scala",
    "dotty/tools/dotc/core/StdNames.scala",
    "dotty/tools/dotc/core/Symbols.scala",
    "dotty/tools/dotc/fromtasty/Debug.scala",
    "dotty/tools/dotc/config/Settings.scala",
    "dotty/tools/dotc/parsing/Parsers.scala",
    "dotty/tools/dotc/sbt/ExtractDependencies.scala",
    "dotty/tools/dotc/core/tasty/CommentPicklingTest.scala",
    "dotty/tools/dotc/printing/PrintingTest.scala",
    "dotty/tools/dotc/transform/PatmatExhaustivityTest.scala",
    "dotty/tools/sbtplugin/DottyPlugin.scala",
    "src/scala/tasty/inspector/TastyInspector.scala"
  )

  final def munitExclusionList = List(
    // Syntax no longer valid in Scala 3
    //xml literals
    "main/scala/docs/MUnitModifier.scala",
    // old given syntax
    "src/main/scala-0.27/munit/internal/MacroCompat.scala"
  )

  final val ignoreParts = List(
    "/tests/",
    "/sbt-test/",
    "/out/",
    "/language-server/src/dotty/"
  )


  // def dottyCommunityBuilds = List(
  //   CommunityBuild("https://github.com/dotty-staging/ScalaPB", None, "ScalaPB", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/scalatest", None, "scalatest", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/scalap", None, "scalap", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/minitest", None, "minitest", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/fastparse", None, "fastparse", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/stdLib213", None, "stdLib213", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/pdbp", None, "pdbp", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/sourcecode", None, "sourcecode", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/scala-xml", None, "scala-xml", Nil),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/shapeless",
  //     Some(
  //       "fb336168a597e159c4861434cf78d41bb76c630e"
  //     ),
  //     "",
  //     Nil
  //   ),
  //   CommunityBuild("https://github.com/dotty-staging/xml-interpolator", None, "interpolator", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/effpi", None, "effpi", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/utest", None, "utest", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/os-lib", None, "os-lib", Nil),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scalatestplus-scalacheck",
  //     None,
  //     "scalatestplus-scalacheck",
  //     Nil
  //   ),
  //   CommunityBuild("https://github.com/dotty-staging/upickle", None, "upickle", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/sconfig", None, "sconfig", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/zio", None, "zio", Nil),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scodec",
  //     Some(
  //       "f2190b4fc99393d6e7ba3220155991c119a5de4e"
  //     ),
  //     "scodec",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scodec-bits",
  //     Some(
  //       "b535556954f0c707f40dc36979c019f7b2f2e8be"
  //     ),
  //     "scodec-bits",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scala-parser-combinators",
  //     None,
  //     "combinators",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/dotty-cps-async",
  //     None,
  //     "dotty-cps-async",
  //     Nil
  //   ),
  //   CommunityBuild("https://github.com/dotty-staging/geny", None, "geny", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/scalaz", None, "", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/endpoints", None, "endpoints4s", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/scas", None, "scas", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/fansi", None, "fansi", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/PPrint", None, "PPrint", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/requests-scala", None, "requests-scala", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/cats-effect-2", None, "cats-effect-2", Nil),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/cats-effect-3",
  //     Some(
  //       "04155e429e230224e61235c4e4dd125419f82f7a"
  //     ),
  //     "cats-effect-3",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scala-parallel-collections",
  //     None,
  //     "scala-parallel-collections",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/scala-collection-compat",
  //     None,
  //     "scala-collection-compat",
  //     Nil
  //   ),
  //   CommunityBuild("https://github.com/dotty-staging/nanotest-strawman.git", None, "verify", Nil),
  //   CommunityBuild("https://github.com/dotty-staging/discipline", None, "discipline", Nil),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/discipline-munit",
  //     None,
  //     "discipline-munit",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/discipline-specs2",
  //     None,
  //     "discipline-specs2",
  //     Nil
  //   ),
  //   CommunityBuild(
  //     "https://github.com/dotty-staging/simulacrum-scalafix",
  //     None,
  //     "simulacrum-scalafix",
  //     Nil
  //   ),
  //   CommunityBuild("https://github.com/dotty-staging/cats", None, "cats", Nil)
  // )
}
