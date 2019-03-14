package net.gumbix.console

import java.io._

import net.gumbix.Settings

/**
  * Create LaTex-Output to emulate a console output.
  * Known issues: Umlauts etc. cannot be handled. If the console
  * output contains umlauts a LaTeX error occurs.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class ConsolePrintln() {

  // Calling a constructor with Scala-default values
  // is not possible from Java :-(.
  var exception: Boolean = true
  val dir = Settings.rootDir + "console\\"
  private var mos = new MemoryOutputStream()

  try {
    val ps = new PrintStream(mos)
    System.setOut(ps) // This does the trick!
    if (exception) {
      System.setErr(ps);
    }
  }
  catch {
    case e: Exception => {
      println("Warning: Console printing not possible.")
    }
  }

  def addStackTrace(e: Exception, maxCalls: Int = 2) {
    val LEN = 70

    def trim(s: String) = {
      if (s.size > LEN) s.take(LEN - 3).mkString + "..." else s
    }

    val m = new MemoryOutputStream()
    val p = new PrintStream(m)
    e.printStackTrace(p) // Print to this stream.
    val s = m.text().split('\n')
    val min = Math.min(s.size, maxCalls)
    if (min > 0) {
      System.err.println()
      System.err.println(trim(s(0)))
      for (i <- 1 to min) {
        System.err.println(" " + trim(s(i)))
      }
      if (min < s.size) {
        System.err.println("  ...")
      }
    }
    m.close()
    p.close()
  }

  /**
    * Write a LaTeX-excerpt to the output directory with
    * a file name (parameter) and the suffix ".con.tex".
    * @param name
    */
  def saveLaTeX(name: String) {
    val ps = new PrintWriter(dir + name + ".con.tex")
    ps.println("\\begin{mdframed}[style=commandline]")
    ps.println("\\begin{verbatim}")
    val s = mos.text().split('\n')
    s.foreach {
      line =>
        ps.println(line)
    }
    ps.println("\\end{verbatim}")
    ps.println("\\end{mdframed}")
    ps.close()
  }
}

class MemoryOutputStream extends OutputStream {
  private val list = new StringWriter

  override def write(b: Int) = b match {
    case '\r' => // Strip extra new line.
    // case 0xC3FF => 252  //'ü' // TODO
    // case 0xBCFF =>
    case _ => list.append(b.asInstanceOf[Char])
  }

  def text() = list.toString
}