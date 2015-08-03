package org.vegas.compiler.semicolon

import org.vegas.compiler.{Compiler, FileCompiler, StaticCompiler}

class SemicolonCompiler extends Compiler {
    def compile(source: String) =
        Some((source.lines.foldLeft("") ({ (program, line) =>
            line.trim.headOption match {
                case Some('.') => program + "\n" + line.stripSuffix(" ")
                case Some(x) => program.trim.lastOption match {
                    case Some('{') => program + "\n" + line.stripSuffix(" ")
                    case Some('+') => program + "\n" + line.stripSuffix(" ")
                    case Some('-') => program + "\n" + line.stripSuffix(" ")
                    case Some('=') => program + "\n" + line.stripSuffix(" ")
                    case Some(',') => program + "\n" + line.stripSuffix(" ")
                    case Some(_) => program + ";\n" + line.stripSuffix(" ")
                    case None => program + line.stripSuffix(" ")
                }
                case None => program + line.stripSuffix(" ")
            }
        }) + ";").replaceAllLiterally(";;", ";"))
}

object SemicolonCompiler extends StaticCompiler {
    implicit lazy val compiler = new SemicolonCompiler()
}

case class SemicolonFileCompiler(val filename: String)
    extends FileCompiler[SemicolonCompiler]("semicolon")
