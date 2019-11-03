package net.gumbix.codesnippet.test

import java.io.File

import net.gumbix.codesnippet.ExtractCodeSnippet
import org.junit.Test
import org.junit.Assert.assertTrue

import scala.io.Source

class ExtractCodeSnippetTest {

  // Where to find the sources and where to save the snippets:
  val srcDir = "src/test/testfiles/src"
  val snippetsPublicTargetDir = "src/test/testfiles/snippetsPublicTarget"
  val snippetsSolutionTargetDir = "src/test/testfiles/snippetsSolutionTarget"
  val srcPublicTargetDir = "src/test/testfiles/srcPublicTarget"
  val srcSolutionTargetDir = "src/test/testfiles/srcSolutionTarget"

  @Test
  def test_OUT_EXC() {
    val filenamePrefix = "Testfile_OUT_EXC"
    performSnippet(filenamePrefix)
    val (p, s) = compareTestfiles(filenamePrefix)
    assertTrue(p)
    assertTrue(s)
  }

  @Test
  def test_EXCSUBST() {
    val filenamePrefix = "Testfile_EXCSUBST"
    performSnippet(filenamePrefix)
    val (p, s) = compareTestfiles(filenamePrefix)
    assertTrue(p)
    assertTrue(s)
  }

  @Test
  def test_Foo_Slide() {
    val filenamePrefix = "Testfile_IN_Slide"
    performSnippet(filenamePrefix)
    val (p, s) = compareTestfiles(filenamePrefix)
    assertTrue(p)
    assertTrue(s)

    // There is one snippet in here:
    val (p2, s2) = compareSnippetTestfiles(filenamePrefix, "Slide")
    assertTrue(p2)
    assertTrue(s2)
  }

  /**
    * Performs the snippets extractor for a given prefix of
    * a file name.
    * @param filenamePrefix prefix, e.g. Testfile01
    * @return A tuple of size 2. First entry: result of public
    *         snippet; second entry: result of solution snippet.
    */
  def performSnippet(filenamePrefix: String) = {
    val filename = filenamePrefix + ".java"
    val file = new File(srcDir + "/" + filename)

    val p = new ExtractCodeSnippet(file, "//",
      snippetsPublicTargetDir, srcPublicTargetDir, false)
    val s = new ExtractCodeSnippet(file, "//",
      snippetsSolutionTargetDir, srcSolutionTargetDir, true)
  }

  def compareTestfiles(filenamePrefix: String) = {
    val filename = filenamePrefix + ".java"
    val filenamePublic = filenamePrefix + "-public.java"
    val filenameSolution = filenamePrefix + "-solution.java"

    val p = compareFiles(srcDir + "/" + filenamePublic,
      srcPublicTargetDir + "/" + filename)
    val s = compareFiles(srcDir + "/" + filenameSolution,
      srcSolutionTargetDir + "/" + filename)
    (p, s)
  }

  def compareSnippetTestfiles(filenamePrefix: String,
                              snippet: String) = {
    val filenamePrefix2 = filenamePrefix + "_" + snippet
    val filename = filenamePrefix2 + ".java"
    val filenamePublic = filenamePrefix2 + "-public.java"
    val filenameSolution = filenamePrefix2 + "-solution.java"

    val p = compareFiles(srcDir + "/" + filenamePublic,
      snippetsPublicTargetDir + "/" + filename)
    val s = compareFiles(srcDir + "/" + filenameSolution,
      snippetsSolutionTargetDir + "/" + filename)
    (p, s)
  }

  /**
    * Compare two text files for equality.
    * @param file1 Filename of first file.
    * @param file2 Filename of second file.
    * @return True if both files are identical, false if not.
    */
  def compareFiles(file1: String, file2: String): Boolean = {
    // Read the text files:
    val lines1 = Source.fromFile(file1).getLines.toList
    val lines2 = Source.fromFile(file2).getLines.toList

    if (lines1.size != lines2.size) {
      false // Different line numbers -> different
    } else {
      // Compare files line-wise, if all lines are identical: true.
      lines1.zip(lines2).map {
        z =>
          z._1 == z._2 // Compare lines from the two files
      }.forall(b => b) // Is every line identical?
    }
  }
}