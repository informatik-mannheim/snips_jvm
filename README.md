# Snippets

Markus Gumbel

Version 0.9.x, May 2021

#### Abstract

Snippets is a tool that supports the presentation and description of programs in documents like presentation, books etc. It aims at teachers who present tons of source code to their audience or authors who write a text book on programming. Snippet can

 * extract snippets (fragments of any source code) which can be included into
  a text document or further processed.

 There is also a support for the Java programming language to

 * redirect the console output of a program which can be included into a text document.
 * record key execution steps and show the protocol in a text document.

Snippets follows the _single source_ principle. You do not copy and paste source code or console output - instead the tool does this for you. Currently, snippets works for all programming languages which have single line comments (like // in Java or # in Python). snippets can also be used for markdown languages like R markdown. The console output and traces is only possible yet for Java-based languages.

## Prerequisites and installation

You need a Java Runtime Environment for Java 8 or higher. Download the jar
 `file snips-core-0.9.x-jar-with-dependencies.jar` and save it where ever you
  want.  The execution of this tool is described later in this section.

## Snippets

### Definition of a snippet

In its simplest form, a snippet is an extract of a source code. Usually, the complete source code of a working example is to long to present it in a listing. Here you may want to create a snippet which highlights the statements you want to explain.

Let us consider the following complete Java program in the file `Foo.java`.

```
public class Foo {
  public static void main(String[] args) {
    // +IN Slide
    int a = 1;
    // -IN Slide
   System.out.println("Value is " + a);
  }
}
```
The statements `+IN` and `-IN` in the comments will create a snippet `Foo_Slide.java` which contains

```
...
    int a = 1;
...
```
The word `Slide` after `+IN` (and `-IN`) indicates a label for this snippet. The tool Snippets simply looks for some keywords in single line commands. You may have noticed that snippets embeds the extract with dots (…) to indicate that not the entire listing is shown.

Of course, we can add more snippets and nest them if needed.

```
public class Foo {
  // +IN Slide_main
  public static void main(String[] args) {
    // +IN Slide_statement
    int a = 1;
    // -IN Slide_statement
   System.out.println("Value is " + a);
  }
  // -IN Slide_main
}
```
This will create to snippets:

1) `Foo_Slide_statement.java`

```
...
    int a = 1;
...
```

2)  `Foo_Slide_main.java`

```
...
  public static void main(String[] args) {
    int a = 1;
    System.out.println("Value is " + a);
  }
...
```

### Hide statements

Sometimes is is useful to hide statements in the snippets. This can be achieved with the `+OUT` and `-OUT` keywords.

```
public class Foo {
  // +IN Slide_main
  public static void main(String[] args) {
    int a = 1;
    // +OUT
    // This will be excluded.
    System.out.println("Hello Word!");
    // -OUT
    System.out.println("Value is " + a);
  }
  // -IN Slide_main
}
```

The snippet `Foo_main.java` still contains

```
...
  public static void main(String[] args) {
    int a = 1;
    System.out.println("Value is " + a);
  }
...
```

Snippet will create the snippets in a specified folder (e. g. `snippets`). Also, snippet will _copy_ all scanned source files to an additional specified folder (e. g. `srcPublic`). Copy means that the source files will not contain any statements which were excluded with the `OUT` option. This is useful if you want to publish your source code but want to exclude several statements. 

### Publishing sample solutions

In a teaching scenario you may want to give your students exercises which contain some source code. However, the solution of the exercise must not be published. There is a snippet option `EXC` (like exercise) which prevents the publication of the solution. `EXC` is similar to `OUT` with the difference that snippet also copies the scanned source files to an additional specified folder (e. g. `srcSolution`) that contains the solutions.

As an example let us assume we ask the students to output the value of the var `a` to the console. The following source code will do this:

```
public class Foo {
  public static void main(String[] args) {
    int a = 1;
    // +OUT
    // This will be excluded.
    System.out.println("Hello Word!");
    // -OUT
    // +EXC
    // This is the solution:
    System.out.println("Value is " + a);
    // -EXC
  }
}
```

The public folder (`srcPublic`) will contain a file `Foo.java` with the content

```
public class Foo {
  public static void main(String[] args) {
    int a = 1;
  }
}
```
The sample solution folder (`srcSolution`) will also contain a file `Foo.java` but with the content

```
public class Foo {
  public static void main(String[] args) {
    int a = 1;
    // This is the solution:
    System.out.println("Value is " + a);
  }
}
```

Note that the excluded statements (option `OUT`) are always missing.

There are situations where the sample solution cannot simply be omitted because then the program would not compile any more. Consider the exercise where we are asked to override the `toString()` method of class `Bar`. This method has a return value and the Java compiler expects to see such a return statement. This solution would not compile:

```
public class Bar {
  public String toString() {
    // Please return "Hello from Bar!"
    // +EXC
    // This is the solution:
    return "Hello from Bar!";
    // -EXC
  }
}
```

The reason is that the public version in `srcPublic` looks like this. 

```
public class Bar {
  public String toString() {
     // Please return "Hello from Bar!"
  }
}
```
The return statement is missing!

We can use a variant of option `EXC` instead. `EXCSUBST` substitutes the hidden block with a single statement.

```
public class Bar {
  public String toString() {
    // Please return "Hello from Bar!"
    // +EXCSUBST 4 return null; // Not yet completed.
    // This is the solution:
    return "Hello from Bar!";
    // -EXCSUBST
  }
}
```
The public version (in `srcPublic`) will then contain

```
public class Bar {
  public String toString() {
    // Please return "Hello from Bar!"
    return null; // Not yet completed.
  }
}
```
which compiles fine. `EXCSUBST` reads the first white space (here `4`) and interprets this as the number of spaces which it will indent the following statement (`return null`). The solution (in `srcSolution`) looks like:

```
public class Bar {
  public String toString() {
    // Please return "Hello from Bar!"
    // This is the solution:
    return "Hello from Bar!";
  }
}
```

###  Markdown documents

This concept can also be applied for text documents which are based on
 pure text like markdown or LaTeX. We will show an example for R markdown
 . The following documents contains some text and a code chunk in R (the left
  directed ' are displayed as normal ' for simplicity).

```
---
title: "Snips R markdown file"
output: html_notebook
---

# header 1

// +EXC
This is the solution. Please note that a # cannot be used as a comment in R markdown as it indicates a header. So // is used instead.
// -EXC

# header 2

This is an example with a code chunk:
'''{r}
a = 1
# +EXC
# This code was removed in the public solution:
print(a)
# -EXC
'''
```

Note that there were two escape comments used. 1) // for markdown and 2
) # for R comments. This is useful because a # in markdown would render a
 header. Thus, any snippet option would be displayed as a header in the
  primary document. Telling snippet to use // as a comment with snippet
   options mitigates this.

The public version (in `srcPublic`) will then contain

```
---
title: "Snips R markdown file"
output: html_notebook
---

# header 1


# header 2

This is an example with a code chunk:
'''{r}
a = 1
'''
```

This hides some text and some programming statements. The solution (in `srcSolution`) looks like:

```
---
title: "Snips R markdown file"
output: html_notebook
---

# header 1

This is the solution. Please note that a # cannot be used as a comment in R markdown as it indicates a header. So // is used instead.

# header 2

This is an example with a code chunk:
'''{r}
a = 1
# This code was removed in the public solution:
print(a)
'''
```

### Execution

#### Publish public source files

The following example scans all Java files (`*.java`) in the directory `src` and writes the snippets to `snippets`. The single line comment is `//` as Java files are scanned.

`java -cp snips-core-0.9.x.jar net.gumbix.codesnippet.SnippetFileCollector
 src snippets srcPublic .java "//" "" False`

The published source code files are written to `srcPublic`. The option `False` tells snippets not to add the solutions. I. e. any statements within `EXC` or `EXCSUBST` are omitted or substituted.

#### Publish solution source files

The following example scans all Java files (`*.java`) in the directory `src` and writes the snippets to `snippets_solution`. Again, the single line comment is `//` as Java files are scanned.

`java -cp snips-core-0.9.x.jar net.gumbix.codesnippet.SnippetFileCollector
 src snippets_solution srcSolution .java "//"  "" True`

The published source code files are written to `srcSolution`. The option `True` tells snippets to add the solutions. I. e. any statements within `EXC` or `EXCSUBST` are included.

### Command line syntax

You allways start the tool by calling Java with
`java -cp snips-core-0.9.x.jar net.gumbix.codesnippet.SnippetFileCollector`

Then the parameters follow. These are:

 * `srcDir` Pathname (absolute or relative) of the source code's root directory.
 * `snippetTargetDir` Directory where the snippets are written to.
 * `srcTargetDir` Directory where the complete but stripped source files are
  written to (with subdirectories for packages).
 * `suffix` Endings of the files to scan. Default is .java
 * `commentEscape` Symbol for the beginning of a comment. Default is //.
 * `commentEscape2`   Alternative symbol for the beginning of a comment. If
  an alternative comment escape is not required set parameter to "".
 * `exerciseEnv` If true, the target directories contain also the solutions
 ; if false, the solutions are excluded. This is controlled by the EXC and
  EXCSUBST option.

## Console output

Details will follow.

## Traces

Details will follow.
