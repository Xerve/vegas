package org.vegas.compiler.semicolon

import org.vegas.compiler.{Compiler, FileCompiler}

class SemicolonCompiler extends Compiler {
    def compile(source: String) =
        Some(source.lines.foldLeft("") ({ (program, line) =>
            line.trim.headOption match {
                case Some('.') => program + "\n" + line.stripSuffix(" ")
                case Some(x) => program.trim.lastOption match {
                    case Some('{') => program + "\n" + line.stripSuffix(" ")
                    case Some('+') => program + "\n" + line.stripSuffix(" ")
                    case Some('-') => program + "\n" + line.stripSuffix(" ")
                    case Some('=') => program + "\n" + line.stripSuffix(" ")
                    case Some(x) => """:(?=([^"\\]*(\\.|"([^"\\]*\\.)*[^"\\]*"))*[^"]*$)""".r.findFirstIn(program.split('\n').last) match {
                            case Some(x) => program + "\n" + line.stripSuffix(" ")
                            case None => program + ";\n" + line.stripSuffix(" ")
                    }
                    case None => program + line.stripSuffix(" ")
                }
                case None => program + line.stripSuffix(" ")
            }
        }) + ";")
}

object SemicolonCompiler {
    implicit lazy val compiler = new SemicolonCompiler()
    def apply() = compiler
}

case class SemicolonFileCompiler(val filename: String)
    extends FileCompiler[SemicolonCompiler]("semicolon")
