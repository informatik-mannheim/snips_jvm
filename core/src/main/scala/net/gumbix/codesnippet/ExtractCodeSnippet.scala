package net.gumbix.codesnippet

import java.io.{PrintWriter, FileOutputStream, OutputStream, File}
import java.util

import scala.io.Source._
import collection.JavaConversions._

/**
  * Processes source code files (Java or text file in general) and
  * cuts off code or text snippets. Text snippets are blocks with a
  * start and an end symbol which is part of of a comment line.
  * Known snippets are listed below. snipID is a unique identifier
  * or label for a snippet, e.g. "Slide". + indicates a start of a
  * block and - the end.
  * <list>
  * </item>// +IN snipID ... // -IN snipID. Includes this block in
  * the output files.
  * </item>// +OUT snipID ... // -OUT snipID. Excludes this block in
  * * the output files.
  * </list>
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  * @param file             The source code to process.
  * @param commentEscape    Comment symbol for a single line comment.
  *                         E.g. "//" in Java or C/C++,
  *                         "#" in Python or R etc.
  * @param snippetTargetDir Directory to store snippets.
  * @param srcTargetDir     Directory to store complete sources
  *                         including packages.
  */
class ExtractCodeSnippet(val file: File,
                         val commentEscape: String,
                         val snippetTargetDir: String,
                         srcTargetDir: String,
                         val exerciseEnv: Boolean) {

  /**
    * Like main constructor except that commentEscape is set to
    * Java-like "//".
    */
  def this(file: File, snippetTargetDir: String,
           fullTargetDir: String, exerciseEnv: Boolean) =
    this(file, "//", snippetTargetDir, fullTargetDir, exerciseEnv)

  process()

  def process() {

    /**
      * Test if the file to be processed is newer at all.
      * The time stamp of the file is compared to the
      * file being generated.
      * @return True if time stamp of file is newer than
      *         processed file or if the processed file
      *         does not exist yet. False if not.
      */
    def testIfNew() = {
      // Time stamp (in ms) when source file was last modified:
      val srcModTime = file.lastModified
      // Target
      val targetSnippetFileName = snippetTargetDir + "/" + file.getName
      val targetSnippetFile = new File(targetSnippetFileName);
      val targetSrcFileName = srcTargetDir + "/" + file.getName
      val targetSrcFile = new File(targetSrcFileName);
      // Both, the snippet and the source file must exist:
      if (targetSnippetFile.exists() && targetSrcFile.exists()) {
        // Time stamp (in ms) when target file was last modified:
        val targetModTime = targetSnippetFile.lastModified
        srcModTime > targetModTime // file newer?
      } else {
        true // This file needs to be (re-)created.
      }
    }

    if (testIfNew()) {
      println(file.getName + " (updated)")
    } else {
      // println(" (not modified)")
      return
    }

    val DEFAULTLABEL = "x8gfz4hd" // just a crazy string.
    /**
      * Key: a label taken from the annotated source code.
      * Values: text buffer to output.
      */
    val outputCollector = new util.HashMap[String, Record]

    var quiet = false // if true, lines are omitted.
    var exerciseQuiet = false // if true, lines are not omitted.
    var counter = 0
    // line counter
    val noLines = fromFile(file).size // of the the source file
    // This is the default snippet extracting the whole source code.
    start(DEFAULTLABEL)

    // Process each line...
    for (line <- fromFile(file).getLines()) {
      counter += 1
      testToken(line) match {
        // see if this line is a token.
        case Some(RegularToken(label, true)) => {
          println(" + " + label) // begin of a code snippet.
          start(label)
          if (outputCollector.get(label).counter <= 1 &&
            counter > 1) {
            // Print ... but not at the beginning of the file
            // or when ... was printed at the end of a code snippet.
            outputCollector.get(label).buffer.append("..." + "\n")
          }
        }
        case Some(RegularToken(label, false)) => {
          println(" - " + label) // end of a code snippet.
          end(label)
          if (counter < noLines) {
            // Print ... but not at the end of the file.
            outputCollector.get(label).buffer.append("..." + "\n")
          }
        }
        case Some(QuietToken(true)) => {
          quiet = true // start to omit output.
        }
        case Some(QuietToken(false)) => {
          quiet = false // end omitting output.
        }
        case Some(ExerciseToken(true)) => {
          exerciseQuiet = true // start to omit output in exercise mod.
        }
        case Some(ExerciseToken(false)) => {
          exerciseQuiet = false // end omitting output in exercise mod.
        }
        case Some(ReplaceToken(text, true)) => {
          for (r <- outputCollector.values()) {
            if (r.active) {
              r.buffer.append(text + "\n")
            }
          }
          quiet = true // prevents to output next line.
        }
        case Some(ReplaceToken(text, false)) => {
          quiet = false // end marker, output is allowed again.
        }
        case Some(ExerciseReplaceToken(text, true)) => {
          if (!exerciseEnv) {
            for (r <- outputCollector.values()) {
              if (r.active) {
                r.buffer.append(text + "\n")
              }
            }
            quiet = true // prevents to output next line.
          }
        }
        case Some(ExerciseReplaceToken(text, false)) => {
          quiet = false // end marker, output is allowed again.
        }
        case _ => {
          if (!quiet && (exerciseEnv || !exerciseQuiet)) {
            // omit lines when in quiet mode.
            // Store line for every code snippet label...
            for (r <- outputCollector.values()) {
              if (r.active) {
                r.buffer.append(line + "\n")
              }
            }
          }
        }
      }
    }
    end(DEFAULTLABEL); // end default code snippet.
    writeFiles()

    def writeFiles() {

      def prefixSuffix(filename: String) = {
        val i = filename.lastIndexOf('.')
        if (i > 0) {
          // i+1 is the dot, which is skipped
          (filename.substring(0, i), filename.substring(i + 1))
        } else {
          (filename, "")
        }
      }

      for ((label, record) <- outputCollector) {

        def writeFile(name: String) {
          val out = new PrintWriter(name)
          out.println(record.buffer.substring(0, record.buffer.length() - 1))
          out.close()
        }

        val (filename, suffix) = prefixSuffix(file.getName)
        label match {
          case DEFAULTLABEL => {
            writeFile(snippetTargetDir + "/" + file.getName)
            writeFile(srcTargetDir + "/" + file.getName)
          }
          case _ => {
            writeFile(snippetTargetDir + "/" + filename + "_" + label + "." + suffix)
          }
        }
      }
    }

    def start(label: String) {
      if (!outputCollector.containsKey(label)) {
        outputCollector.put(label, Record(true))
      } else {
        outputCollector.get(label).active = true
        outputCollector.get(label).counter += 1
      }
    }

    def end(label: String) {
      if (!outputCollector.containsKey(label)) {
        new RuntimeException("End without start.")
      } else {
        outputCollector(label).active = false
      }
    }

    // Test if line is token. If so, return the appropriate token.
    def testToken(line: String): Option[Token] = {
      val tokens = line.trim.split(" ")
      if (tokens.size == 2) {
        if (tokens(0) == commentEscape && tokens(1) == "+OUT") {
          return Some(QuietToken(true))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-OUT") {
          return Some(QuietToken(false))
        }
        if (tokens(0) == commentEscape && tokens(1) == "+EXC") {
          return Some(ExerciseToken(true))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-EXC") {
          return Some(ExerciseToken(false))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-EXCSUBST") {
          return Some(ExerciseReplaceToken("", false))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-HEADER") {
          return Some(ReplaceToken("", false))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-VAR") {
          return Some(ReplaceToken("", false))
        }
      }
      if (tokens.size >= 3) {
        if (tokens(0) == commentEscape && tokens(1) == "+IN") {
          return Some(RegularToken(tokens(2), true))
        }
        if (tokens(0) == commentEscape && tokens(1) == "-IN") {
          return Some(RegularToken(tokens(2), false))
        }
        if (tokens(0) == commentEscape && tokens(1) == "+HEADER") {
          val o = for (i <- 2 until tokens.size) yield {
            tokens(i)
          }
          return Some(ReplaceToken(o.mkString(" "), true))
        }
        if (tokens(0) == commentEscape && tokens(1) == "+VAR") {
          val indent = tokens(2).toInt
          val o = for (i <- 3 until tokens.size) yield {
            tokens(i)
          }
          val spaces = (1 to indent).map(c => ' ').mkString
          return Some(ReplaceToken(spaces + o.mkString(" "), true))
        }
        if (tokens(0) == commentEscape && tokens(1) == "+EXCSUBST") {
          val indent = tokens(2).toInt
          val o = for (i <- 3 until tokens.size) yield {
            tokens(i)
          }
          val spaces = (1 to indent).map(c => ' ').mkString
          return Some(ExerciseReplaceToken(spaces + o.mkString(" "), true))
        }
      }
      None
    }
  }
}

class Token(start: Boolean)

// label: name of the token, start: start or end?
case class RegularToken(label: String, start: Boolean) extends Token(start)

// start: start or end?
case class QuietToken(start: Boolean) extends Token(start)

// start: start or end?
case class ExerciseToken(start: Boolean) extends Token(start)

// s: next line is replaced with this text, start: start or end?
case class ReplaceToken(s: String, start: Boolean) extends Token(start)

// s: next line is replaced with this text, start: start or end?
case class ExerciseReplaceToken(s: String, start: Boolean) extends Token(start)

/**
  *
  * @param active  true if lines are printed.
  * @param counter number of code snippets (until now)
  * @param buffer  buffer to collect the output text.
  */
case class Record(var active: Boolean, var counter: Int = 1, buffer: StringBuffer = new StringBuffer())
