package net.gumbix.codesnippet

import java.io.File

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.io.Source

/**
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class SnippetUsage(val snippetDir: List[String],
                   val includeDir: String) {

  val snippets = {
    val allSnippets = for (dir <- snippetDir) yield {
      val collector = new SnippetCollector(dir, ".tex")
      collector.snippets
    }
    allSnippets.flatten.toSet
  }

  val includes = new IncludeCollector(includeDir, ".lyx").snippets

  //println(includes.snippets.mkString("\n"))
  println("Number of snippets: " + snippets.size)
  println("Number of includes: " + includes.size)

  val common = snippets.intersect(includes)
  println("Number of shares: " + common.size)
  val common2 = includes.intersect(snippets)
  println("Number of shares2: " + common2.size)
  val diffs = includes.diff(common2)
  println("Number of differences: " + diffs.size)
  println(diffs.mkString("\n"))
  println("done")

}

abstract class AbstractSnippetCollector(val dir: String,
                                        val suffix: String) {

  def snippetsFromFile(file: File): Set[String]

  def scanRec(dir: File, snippets: mutable.Set[String]) {
    val allFiles = dir.listFiles().filter(f => !f.isDirectory &&
      f.getName.endsWith(suffix))
    for (file <- allFiles) {
      // Extract snippet information:
      val s = snippetsFromFile(file)
      // println(s)
      snippets ++= s // Add snippets in file to all snippets.
    }
    val dirs = dir.listFiles().filter(_.isDirectory)
    for (dir <- dirs) {
      // Append this directory to the suffix path:
      scanRec(dir, snippets)
    }
  }

  val snippets = {
    val snippets = new mutable.HashSet[String]()
    scanRec(new File(dir), snippets)
    snippets
  }
}

class SnippetCollector(dir: String, suffix: String) extends
  AbstractSnippetCollector(dir, suffix) {
  def snippetsFromFile(file: File): Set[String] = {
    val filename = file.getName
    val parts = filename.split('.')
    HashSet(parts(0)) // Just first token
  }
}

class IncludeCollector(dir: String, suffix: String) extends
  AbstractSnippetCollector(dir, suffix) {
  def snippetsFromFile(file: File): Set[String] = {
    val snippets = Source.fromFile(file).getLines.filter{
      line => line.startsWith("filename \"listings")
    }
    val fsnippets = snippets.map{
      // Remove prefix 'filename "listings/' and suffix '.java.tex'
      line => line.substring(19, line.size - 10)
    }
    fsnippets.toSet
  }
}

object SnippetUsage {
  def main(args: Array[String]) {
    new SnippetUsage(List("c:\\Users\\Markus\\Local-Docs\\Professur-HS-Mannheim\\Vorlesungen\\PR2\\Script-Slides\\listings",
      "c:\\Users\\Markus\\Local-Docs\\Professur-HS-Mannheim\\Vorlesungen\\PR2\\Script-Slides\\listings_static"),
    "c:\\Users\\Markus\\Local-Docs\\Professur-HS-Mannheim\\Vorlesungen\\PR2\\Script-Slides")
  }
}