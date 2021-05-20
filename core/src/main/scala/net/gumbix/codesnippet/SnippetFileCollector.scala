package net.gumbix.codesnippet

import java.io.File
import java.util.Date

/**
 * Collects recursively all files with a specific suffix starting
 * from a start directory. It calls a code snippet extractor for
 * further processing.
 *
 * @param srcDir           Pathname of the source code's root directory.
 * @param snippetTargetDir Directory where the snippets are written to.
 * @param srcTargetDir     Directory where the complete but stripped
 *                         source files are written to
 *                         (with subdirectories for packages).
 * @param suffix           Endings of the files (by default .java)
 * @param commentEscape    Symbol for the beginning of a comment. Default is //.
 * @param commentEscape2   Alternative symbol for the beginning of a comment. Not used by default.
 * @param exerciseEnv      If true, the public directory contains
 *                         also the solutions; if false, the
 *                         solutions are excluded. This is controlled
 *                         by the EXC and EXCSUBST option.
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 *         (c) 2015-21 Markus Gumbel
 */
class SnippetFileCollector(val srcDir: String,
                           val snippetTargetDir: String,
                           val srcTargetDir: String,
                           val suffix: String = ".java",
                           val commentEscape: String = "//",
                           val commentEscape2: String = "",
                           val exerciseEnv: Boolean) {

  val blackList = Set()

  def scanRec(dir: File, suffixPath: String) {

    def createDirIfNotExists(newDir: String) {
      val dirFile = new File(newDir) // Access to file.
      if (!(dirFile.exists() && dirFile.isDirectory)) {
        dirFile.mkdir() // Directory did not exist, create it.
      }
    }

    val fullTargetDirPackage = srcTargetDir + suffixPath
    val allFiles = dir.listFiles().filter(f => !f.isDirectory && f.getName.endsWith(suffix))
    for (file <- allFiles) {
      new ExtractCodeSnippet(file, commentEscape, commentEscape2, snippetTargetDir,
        fullTargetDirPackage, exerciseEnv)
    }
    val dirs = dir.listFiles().filter(_.isDirectory)
    for (dir <- dirs) {
      // Append this directory to the suffix path:
      val newSuffixPath = suffixPath + "/" + dir.getName
      createDirIfNotExists(srcTargetDir + newSuffixPath)
      scanRec(dir, newSuffixPath)
    }
  }

  val mode = if (exerciseEnv) "(mode EXC) " else ""
  print("Scanning " + mode + srcDir)
  scanRec(new File(srcDir), "") // no suffix path at beginning.
  println(" ... done")
}

object SnippetFileCollector {
  def main(args: Array[String]) {
    if (args.size == 0) {
      new SnippetFileCollector("c:\\Users\\Markus\\Local-Docs\\src\\jvm\\PR2\\public\\src\\main\\java",
        "c:\\Users\\Markus\\Local-Docs\\Professur-HS-Mannheim\\Vorlesungen\\PR2\\Script-Slides\\snippets",
        "c:\\Users\\Markus\\Local-Docs\\src\\jvm\\PR2-gen\\publicExercise",
        ".java", "//", "", true)
    } else if (args.size == 7) {
      new SnippetFileCollector(args(0), args(1), args(2), args(3), args(4), args(5), args(6).toBoolean)
    } else {
      print("usage: SnippetFileCollector <srcDir> <snippetTargetDir> <srcTargetDir> <file suffix> <commentEscape> <commentEscape2> <exercise env>")
    }
  }
}
