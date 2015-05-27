package org.vegas.compiler.semicolon

import org.vegas.compiler.{Compiler, FileCompiler}

class SemicolonCompiler extends Compiler {
    def compile(source: String) =
        Some(source.lines.foldLeft("") ({ (program, line) =>
            line.trim.headOption match {
                case Some('.') => program + "\n" + line
                case Some(x) => program.trim.lastOption match {
                    case Some('{') => program + "\n" + line
                    case Some('+') => program + "\n" + line
                    case Some('-') => program + "\n" + line
                    case Some('=') => program + "\n" + line
                    case Some(x) => program + ";\n" + line
                    case None => program + line
                }
                case None => program + line
            }
        }) + ";")
}

object SemicolonCompiler {
    implicit lazy val compiler = new SemicolonCompiler()
    def apply() = compiler
}

case class SemicolonFileCompiler(val filename: String)
    extends FileCompiler[SemicolonCompiler]("semicolon")
