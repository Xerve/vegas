package org.vegas.compiler.apply

import org.vegas.compiler.{Compiler, FileCompiler}

class ApplyCompiler extends Compiler {
    val functionCall = """([\w\.]+)(\([\s\S]+\))""".r
    val replacementCall = """$1 \$apply $2"""

    def compile(source: String) =
        Some(replaceCalls(source))

    def replaceCalls(source: String): String =
        functionCall findAllIn source match {
            case matches if matches.nonEmpty =>
                replaceCalls(functionCall replaceAllIn (source, replacementCall))
            case matches =>
                source
        }
}

object ApplyCompiler {
    implicit lazy val compiler = new ApplyCompiler()
    def apply() = compiler
}

case class ApplyFileCompiler(val filename: String)
    extends FileCompiler[ApplyCompiler]("apply")
