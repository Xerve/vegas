package org.vegas.compiler.stringReplace

import org.vegas.compiler.Compiler

class StringLoader extends Compiler {
    val stringContent = """\"(\\.|[^\"])*\"""".r

    def compile(source: String) =
        Some(stringContent replaceAllIn (source, { content =>
            Compiler.stringStorage push content.toString()
            "\"STRING_CONTENT_" + Compiler.stringStorage.length.toString() + "\""
        }))
}

object StringLoader {
    def apply() = new StringLoader()
}
