package org.vegas.compiler.stage2

import org.vegas.compiler.{Compiler, FileCompiler}

class Stage2Compiler extends Compiler {
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

object Stage2Compiler {
    implicit lazy val compiler = new Stage2Compiler()
    def apply() = compiler
}

case class Stage2FileCompiler(val filename: String)
    extends FileCompiler[Stage2Compiler]("stage2")
