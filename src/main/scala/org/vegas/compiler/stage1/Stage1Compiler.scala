package org.vegas.compiler.stage1

import org.vegas.compiler.{Compiler, FileCompiler}

class Stage1Compiler extends Compiler {
    def compile(source: String) =
        Some(source.lines.foldLeft(Tuple2(0, "")) ({ (program, line) =>
            val indentation = (line.indexWhere (_ != ' ') / 4)
            val currentIndentation = program._1
            Tuple2(indentation, indentation match {
                case x if x == currentIndentation + 1 => program._2 + " {\n" + line
                case x if x < currentIndentation => program._2 + "\n" + (" " * (4 * indentation)) + "}\n" + line
                case x if x == currentIndentation => program._2 + "\n" + line
                case _ => println("Impropper indentation detected!"); "ERROR"
            })
        })._2.drop(1))
}

object Stage1Compiler {
    implicit lazy val compiler = new Stage1Compiler()
    def apply() = compiler
}

case class Stage1FileCompiler(val filename: String)
    extends FileCompiler[Stage1Compiler]("stage1")
