package org.vegas.compiler

import org.vegas.log

class BracesCompiler extends Compiler {
    def compile(source: String) = {
        val precompiled = source.lines.filter(_.trim.nonEmpty) ++ Seq("\n\n")

        val (_, program) = precompiled.foldLeft(0 -> "") { (program, line) =>
            val lineIndentation = (line.indexWhere (_ != ' ') / 4)
            val (bodyIndentation, body) = program

            val nextBody = lineIndentation match {
                case _ if body.trim.endsWith("[") => body + "\n" + line
                case _ if line.trim.endsWith("]") => body + "\n" + line
                case x if x == bodyIndentation + 1 => body + " {\n" + line
                case x if x < bodyIndentation =>
                    body + "\n" +
                    bodyIndentation.to(lineIndentation, -1).map(ident => "    " * (ident - 1)).mkString("}\n") +
                    line
                case x if x == bodyIndentation => body + "\n" + line
                case _ => throw new exception.UnexpectedIndentationException("Unexpected indentation!")
            }

            lineIndentation -> nextBody
        }

        Some(program.drop(1).trim)
    }
}

object BracesCompiler extends StaticCompiler {
    lazy val compiler = new BracesCompiler()
}
