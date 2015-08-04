package org.vegas.compiler

class BracesCompiler extends Compiler {
    def compile(source: String) =
        Some((source + "\n\n").lines.foldLeft(Tuple2(0, "")) ({ (program, line) =>
            val indentation = (line.indexWhere (_ != ' ') / 4)
            val currentIndentation = program._1
            Tuple2(indentation, indentation match {
                case _ if program._2.trim.endsWith("[") => program._2 + "\n" + line
                case _ if line.trim.endsWith("]") => program._2 + "\n" + line
                case x if x == currentIndentation + 1 => program._2 + " {\n" + line
                case x if x < currentIndentation => program._2 + "\n" + (" " * (4 * indentation)) + "}\n" + line
                case x if x == currentIndentation => program._2 + "\n" + line
                case _ => println("Impropper indentation detected!"); "ERROR"
            })
        })._2.drop(1).trim)
}

object BracesCompiler extends StaticCompiler {
    lazy val compiler = new BracesCompiler()
}
