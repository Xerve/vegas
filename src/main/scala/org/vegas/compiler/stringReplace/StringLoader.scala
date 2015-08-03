package org.vegas.compiler.stringReplace

import org.vegas.compiler.{Compiler, StaticCompiler}

class StringLoader extends Compiler {
    val stringContent = """\"(\\.|[^\"])*\"""".r

    def compile(source: String) =
        Some(stringContent replaceAllIn (source, { content =>
            Compiler.stringStorage += content.toString()
            "\"STRING_CONTENT_" + Compiler.stringStorage.length.toString() + "\""
        }))
}

object StringLoader extends StaticCompiler {
    val compiler  = new StringLoader()
}
