package net.gumbix.codesnippet

import java.io.File
import java.util.Date

/**
  * Collects recursively all files with a specific suffix starting from a
  * start directory. It calls a code snippet extractor for further
  * processing.
  * @param srcDir           Pathname of the source code's root directory.
  * @param snippetTargetDir Directory where the snippets are written to.
  * @param fullTargetDir    Directory where the full but stripped file
  *                         is written to (with subdirectories for
  *                         packages)
  * @param suffix           Endings of the files (by default .java)
  * @param exerciseEnv      ???
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  *         (c) 2015 Markus Gumbel
  */
class FileCollector(val srcDir: String, val snippetTargetDir: String,
                    val fullTargetDir: String, val suffix: String = ".java",
                    val exerciseEnv: Boolean) {

  val blackList = Set()

  def scanRec(dir: File, suffixPath: String) {

    def createDirIfNotExists(newDir: String) {
      val dirFile = new File(newDir) // Access to file.
      if (!(dirFile.exists() && dirFile.isDirectory)) {
        dirFile.mkdir() // Directory did not exist, create it.
      }
    }

    val fullTargetDirPackage = fullTargetDir + suffixPath
    val allFiles = dir.listFiles().filter(f => !f.isDirectory && f.getName.endsWith(suffix))
    for (file <- allFiles) {
      new ExtractCodeSnippet(file, snippetTargetDir,
        fullTargetDirPackage, exerciseEnv);
    }
    val dirs = dir.listFiles().filter(_.isDirectory)
    for (dir <- dirs) {
      // Append this directory to the suffix path:
      val newSuffixPath = suffixPath + "/" + dir.getName
      createDirIfNotExists(fullTargetDir + newSuffixPath)
      scanRec(dir, newSuffixPath)
    }
  }

  scanRec(new File(srcDir), "") // no suffix path at beginning.
  println("done")
}

object FileCollector {
  def main(args: Array[String]) {
    if (args.size == 0) {
      new FileCollector("c:\\Users\\Markus\\Local-Docs\\src\\jvm\\PR2\\public\\src\\main\\java",
        "c:\\Users\\Markus\\Local-Docs\\Professur-HS-Mannheim\\Vorlesungen\\PR2\\Script-Slides\\snippets",
        "c:\\Users\\Markus\\Local-Docs\\src\\jvm\\PR2-gen\\public",
        ".java", true)
    } else if (args.size == 5) {
      new FileCollector(args(0), args(1), args(2), args(3), args(4).toBoolean)
    } else {
      print("usage: FileCollector <src> <snippetTargetDir> <fullTargetDir> <suffix>")
    }
  }
}
