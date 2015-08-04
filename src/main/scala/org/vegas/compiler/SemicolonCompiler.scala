package org.vegas.compiler

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
                    case Some('[') => program + "\n" + line.stripSuffix(" ")
                    case Some(_) => program + ";\n" + line.stripSuffix(" ")
                    case None => program + line.stripSuffix(" ")
                }
                case None => program + line.stripSuffix(" ")
            }
        }) + ";").replaceAllLiterally(";;", ";"))
}

object SemicolonCompiler extends StaticCompiler {
    lazy val compiler = new SemicolonCompiler()
}
