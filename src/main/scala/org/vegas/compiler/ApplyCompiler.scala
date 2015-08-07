package org.vegas.compiler

class ApplyCompiler extends Compiler {
    val functionCall = """([\w\.\!]+)\(([\s\S]*?)\)""".r
    val replacementCall = """$1 \$apply ($2)"""

    def compile(source: String) =
        Some(replaceCalls(source))

    def replaceCalls(source: String): String = {
        val newSource = functionCall replaceAllIn (source, replacementCall)
        if (newSource == source) source else replaceCalls(newSource)
    }
}

object ApplyCompiler extends StaticCompiler {
    lazy val compiler = new ApplyCompiler()
}
