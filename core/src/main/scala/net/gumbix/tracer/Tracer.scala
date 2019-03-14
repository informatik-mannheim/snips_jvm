package net.gumbix.tracer

import java.io.PrintWriter
import java.util

import net.gumbix.Settings

import scala.collection.immutable.HashMap
import scala.collection.mutable.HashSet
import collection.JavaConversions._

/**
  * TODO logArray and logObjectArray is redundant.
  * Maybe this can resolved.
  * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
  */
class Tracer {

  val TABSIZE = 1
  val targetDir = Settings.rootDir + "\\traces\\"
  private var level = 0

  var out = new StringBuffer()

  def reset() {
    out = new StringBuffer()
  }

  def save(filename: String) = {
    val pw = new PrintWriter(targetDir + "/" + filename + ".tex");
    pw.println("\\noindent")
    pw.println(out.toString)
    pw.close()
  }

  var flat = false
  var showVars = true

  private var includeTags = HashSet[String]()

  def addInclude(tag: String) {
    includeTags += tag
  }

  private var scopes = HashSet[String]()

  def addTagScope(scope: String) {
    scopes += scope
  }

  def clearTagScopes() {
    scopes.clear
  }

  def logComment(s: String) {
    printlnOut("\\color[rgb]{0.5,0.6,0.5}//\\quad " + s + "\\color{black}")
  }

  // Generic with Object array:
  def logObjectArrayVar(a: Array[Object]) {
    logObjectArrayVar(a, HashMap(), Set())
  }

  def logObjectArrayVar(v: String, a: Array[Object]) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logObjectArrayVar(a, HashMap(), Set())
  }

  def logObjectArrayVarRange(a: Array[Object], from: Int, to: Int) {
    logObjectArrayVar(a, HashMap(), (from to to).toSet)
  }

  def logObjectArrayVarRange(v: String, a: Array[Object], from: Int, to: Int) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logObjectArrayVar(a, HashMap(), (from to to).toSet)
  }

  def logObjectArrayVarUnderline(a: Array[Object], i: Int, j: Int) = {
    logObjectArrayVar(a, HashMap(i -> "underline", j -> "underline"), Set())
  }

  def logObjectArrayVarUnderlineRange(a: Array[Object], i: Int,
                                from: Int, to: Int) = {
    logObjectArrayVar(a, HashMap(i -> "underline"), (from to to).toSet)
  }

  def logObjectArrayVarUnderlineRange(v: String, a: Array[Object], i: Int,
                                from: Int, to: Int) = {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logObjectArrayVar(a, HashMap(i -> "underline"), (from to to).toSet)
  }

  def logObjectArrayVarUnderlineRange(v: String, a: Array[Object], i: Int, j: Int,
                                from: Int, to: Int) = {
    if (showVars) printI("\\tab{$" + v + " = $}")
    val s = (from to to).toSet
    logObjectArrayVar(a, HashMap(i -> "underline", j -> "underline"), s)
  }

  def logObjectArrayVarUnderlineRange(a: Array[Object], i: Int, j: Int,
                                from: Int, to: Int) = {
    val s = (from to to).toSet
    logObjectArrayVar(a, HashMap(i -> "underline", j -> "underline"), s)
  }

  def logObjectArrayVarRange(v: String, a: Array[Object], background: java.util.Set[Int]) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logObjectArrayVar(a, HashMap(), background.toSet)
  }

  def logObjectArrayVar(a: Array[Object], markups: Map[Int, String],
                  background: Set[Int]) {
    val jlist: util.List[Object] = util.Arrays.asList(a: _*)
    // 6pt should be fine for two digits:
    val h = (0 until jlist.size()).map(e => "m{1em}").mkString("|", "|", "|")
    printOut("\\begin{tabular}{" + h + "} \\hline")
    val l1 = (0 until jlist.size()).map {
      e => "\\color{gray!50}\\textscale{-2}{" + e + "}\\color{black}"
    }.mkString(" & ")
    printOut(l1 + " \\\\")
    val output = for (c <- 0 until jlist.size()) yield {
      val m = if (markups.contains(c)) "\\color{red}\\" + markups(c) +
        "{" + jlist.get(c) + "}\\color{black}"
      else {
        if (jlist.get(c) == -1) "\\color{blue}{" + jlist.get(c) + "}\\color{black}"
        else jlist.get(c)
      }
      if (background.contains(c)) "\\cellcolor{green!40}" + m else m
    }
    printOut(output.mkString(" & ") + " \\\\ \\hline") //
    printlnOut("\\end{tabular}")
  }

  // Generic with int array:
  def logArrayVar(a: Array[Int]) {
    logArrayVar(a, HashMap(), Set())
  }

  def logArrayVar(v: String, a: Array[Int]) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logArrayVar(a, HashMap(), Set())
  }

  def logArrayVarRange(a: Array[Int], from: Int, to: Int) {
    logArrayVar(a, HashMap(), (from to to).toSet)
  }

  def logArrayVarRange(v: String, a: Array[Int], from: Int, to: Int) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logArrayVar(a, HashMap(), (from to to).toSet)
  }

  def logArrayVarUnderline(a: Array[Int], i: Int, j: Int) = {
    logArrayVar(a, HashMap(i -> "underline", j -> "underline"), Set())
  }

  def logArrayVarUnderlineRange(a: Array[Int], i: Int,
                                from: Int, to: Int) = {
    logArrayVar(a, HashMap(i -> "underline"), (from to to).toSet)
  }

  def logArrayVarUnderlineRange(v: String, a: Array[Int], i: Int,
                                from: Int, to: Int) = {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logArrayVar(a, HashMap(i -> "underline"), (from to to).toSet)
  }

  def logArrayVarUnderlineRange(v: String, a: Array[Int], i: Int, j: Int,
                                from: Int, to: Int) = {
    if (showVars) printI("\\tab{$" + v + " = $}")
    val s = (from to to).toSet
    logArrayVar(a, HashMap(i -> "underline", j -> "underline"), s)
  }

  def logArrayVarUnderlineRange(a: Array[Int], i: Int, j: Int,
                                from: Int, to: Int) = {
    val s = (from to to).toSet
    logArrayVar(a, HashMap(i -> "underline", j -> "underline"), s)
  }

  def logArrayVarRange(v: String, a: Array[Int], background: java.util.Set[Int]) {
    if (showVars) printI("\\tab{$" + v + " = $}")
    logArrayVar(a, HashMap(), background.toSet)
  }

  def logArrayVar(a: Array[Int], markups: Map[Int, String],
                  background: Set[Int]) {
    val jlist: util.List[Int] = util.Arrays.asList(a: _*)
    // 6pt should be fine for two digits:
    val h = (0 until jlist.size()).map(e => "m{1em}").mkString("|", "|", "|")
    printOut("\\begin{tabular}{" + h + "} \\hline")
    val l1 = (0 until jlist.size()).map {
      e => "\\color{gray!50}\\textscale{-2}{" + e + "}\\color{black}"
    }.mkString(" & ")
    printOut(l1 + " \\\\")
    val output = for (c <- 0 until jlist.size()) yield {
      val m = if (markups.contains(c)) "\\color{red}\\" + markups(c) +
        "{" + jlist.get(c) + "}\\color{black}"
      else {
        if (jlist.get(c) == -1) "\\color{blue}{" + jlist.get(c) + "}\\color{black}"
        else jlist.get(c)
      }
      if (background.contains(c)) "\\cellcolor{green!40}" + m else m
    }
    printOut(output.mkString(" & ") + " \\\\ \\hline") //
    printlnOut("\\end{tabular}")
  }

  // end

  def logVar(a: String, v: Any) {
    if (showVars) printlnI("$" + a + " = $ " + v) else printlnI(v.toString)
  }

  def logVarComment(a: String, v: Any, c: String) {
    if (showVars) {
      printlnI("$" + a + " = $ " + v + "\\color[rgb]{0.5,0.6,0.5}\\quad// " + c + "\\color{black}")
    } else {
      printlnI(v + "\\color[rgb]{0.5,0.6,0.5}\\quad// " + c + "\\color{black}")
    }
  }

  def logTree(a: String, n: String) {
    if (showVars) {
      printlnI("$" + a + " = $\n" + n)
    } else {
      printlnI(n)
    }
  }

  def logOutput(s: String) {
    printlnI("\\color{blue!70}Ausgabe ''" + s + "'' \\color{black}")
  }

  def logInput(a: String, v: Any) {
    if (showVars) {
      printlnI("$" + a + " = $ " + v + "\\color{gray!50}~(von Eingabe) \\color{black}")
    } else {
      printlnI(v + "\\color{gray!50}~(von Eingabe) \\color{black}")
    }
  }

  def call(fct: String) {
    callI(fct + "()")
  }

  def call(fct: String, param: Any) {
    callI(toFunction(fct, List(param)))
  }

  def callNamed(fct: String, name: String, param: Any) {
    callI(toNamedFunction(fct, List(name), List(param)))
  }

  def call(fct: String, param1: Any, param2: Any) {
    callI(toFunction(fct, List(param1, param2)))
  }

  def callNamed(fct: String, name1: String, param1: Any, name2: String, param2: Any) {
    callI(toNamedFunction(fct, List(name1, name2), List(param1, param2)))
  }

  def call(fct: String, param1: Any, param2: Any, param3: Any) {
    callI(toFunction(fct, List(param1, param2, param3)))
  }

  def call(fct: String, param1: Any, param2: Any, param3: Any, param4: Any) {
    callI(toFunction(fct, List(param1, param2, param3, param4)))
  }

  def callNamed(fct: String, name1: String, param1: Any, name2: String, param2: Any, name3: String, param3: Any) {
    callI(toNamedFunction(fct, List(name1, name2, name3), List(param1, param2, param3)))
  }

  def callNamed(fct: String, name1: String, param1: Any, name2: String, param2: Any
                , name3: String, param3: Any, name4: String, param4: Any) {
    callI(toNamedFunction(fct, List(name1, name2, name3, name4),
      List(param1, param2, param3, param4)))
  }

  def callI(s: String) {
    if (!flat) printlnI("\\color[rgb]{0.2,0.4,0.0}$" + s + "\\searrow$ \\color{black}")
    level += 1
  }

  def ret(fct: String, res: Any) {
    retI(fct + "() = " + res)
  }

  def ret(fct: String, res: Any, param: Any) {
    retI(toFunction(fct, List(param)) + " = " + res)
  }

  def ret(fct: String, res: Any, param1: Any, param2: Any) {
    retI(toFunction(fct, List(param1, param2)) + " = " + res)
  }

  def ret(fct: String, res: Any, param1: Any, param2: Any, param3: Any) {
    retI(toFunction(fct, List(param1, param2, param3)) + " = " + res)
  }

  def retVoid(fct: String) {
    retI(fct + "()")
  }

  def retVoid(fct: String, param: Any) {
    retI(toFunction(fct, List(param)))
  }

  def retVoid(fct: String, param1: Any, param2: Any) {
    retI(toFunction(fct, List(param1, param2)))
  }

  def retVoid(fct: String, param1: Any, param2: Any, param3: Any) {
    retI(toFunction(fct, List(param1, param2, param3)))
  }

  def retVoid(fct: String, param1: Any, param2: Any, param3: Any, param4: Any) {
    retI(toFunction(fct, List(param1, param2, param3, param4)))
  }

  def retI(s: String) {
    level -= 1
    if (!flat) printlnI("\\color[rgb]{0.2,0.4,0.0}$" + s + "\\swarrow$ \\color{black}")
  }

  def printI(s: String) {
    printOut(indent.toString + s.toString)
  }

  def printlnI(s: String) {
    printlnOut(indent.toString + s.toString)
  }

  def printlnOut(s: String) {
    if (scopes.isEmpty || !scopes.intersect(includeTags).isEmpty) {
      out.append(s + " \\newline \n")
    }
  }

  def printOut(s: String) {
    if (scopes.isEmpty || !scopes.intersect(includeTags).isEmpty) {
      out.append(s + " \n")
    }
  }

  def toFunction(fct: String, params: List[Any]) = fct + params.mkString("(", ", ", ")")

  def toNamedFunction(fct: String, names: List[String], params: List[Any]) = {
    val pairs = (names zip params).map { e => e._1.toString + " = " + e._2.toString }
    fct + pairs.mkString("(", ", ", ")")
  }

  def indent() = if (level == 0 || flat) "" else (1 to level).map(c => tab).mkString("")

  //val tab = (1 to TABSIZE).map(c => "\\quad ").mkString("")
  val tab = (1 to TABSIZE).map(c => "\\color{gray!50} \\textunderscore ~ \\color{black}").mkString("")
}