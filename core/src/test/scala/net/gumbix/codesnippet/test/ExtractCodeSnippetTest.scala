package net.gumbix.codesnippet.test

import java.io.File

import net.gumbix.codesnippet.ExtractCodeSnippet
import org.junit.Test
import org.junit.Assert.assertTrue

import scala.io.Source

class ExtractCodeSnippetTest {

  // Where to find the sources and to save the snippets:
  val srcDir = "src/test/testfiles/src"
  val snippetsTargetDir = "src/test/testfiles/snippetsTarget"
  val srcPublicTargetDir = "src/test/testfiles/srcPublicTarget"
  val srcSolutionTargetDir = "src/test/testfiles/srcSolutionTarget"

  @Test
  def test_OUT_EXC() {
    val (p, s) = performSnippet("Testfile_OUT_EXC")
    assertTrue(p)
    assertTrue(s)
  }

  @Test
  def test_Foo_Slide() {
    val filename = srcDir + "/" + "Testfile_IN_Slide.java"
    val filenameSnippet = snippetsTargetDir + "/" +
      "Testfile_IN_Slide_Slide.java"
    val filenamePublicSnippet = srcDir + "/" +
      "Testfile_IN_Slide_Slide-public.java"

    val file = new File(filename)
    val s = new ExtractCodeSnippet(file, "//",
      snippetsTargetDir, srcPublicTargetDir, false)
    assertTrue(compareFiles(filenameSnippet, filenamePublicSnippet))
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
    val filenamePublic = filenamePrefix + "-public.java"
    val filenameSolution = filenamePrefix + "-solution.java"

    val file = new File(srcDir + "/" + filename)

    val p = new ExtractCodeSnippet(file, "//",
      snippetsTargetDir, srcPublicTargetDir, false)
    val s = new ExtractCodeSnippet(file, "//",
      snippetsTargetDir, srcSolutionTargetDir, true)

    val pr = compareFiles(srcDir + "/" + filenamePublic,
      srcPublicTargetDir + "/" + filename)
    val sr = compareFiles(srcDir + "/" + filenameSolution,
      srcSolutionTargetDir + "/" + filename)
    (pr, sr)
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