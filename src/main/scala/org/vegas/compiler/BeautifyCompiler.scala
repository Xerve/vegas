package org.vegas.compiler

class BeautifyCompiler extends Compiler {
    def compile(source: String) = Some(fixIndentation(source))

    def fixIndentation(source: String) = {
        val (_, beautified) = source.lines.foldLeft(0 -> "") { (program, line) =>
            val (currentIndentation, body) = program
            val nextIndentation = line match {
                case _ if line endsWith "}" => currentIndentation - 1
                case _ if line endsWith "{" => currentIndentation + 1
                case _ => currentIndentation
            }

            val source = body + ("    " * currentIndentation.min(nextIndentation)) + line + "\n"

            nextIndentation -> source
        }

        beautified
    }
}

object BeautifyCompiler extends StaticCompiler {
    lazy val compiler = new BeautifyCompiler()
}
