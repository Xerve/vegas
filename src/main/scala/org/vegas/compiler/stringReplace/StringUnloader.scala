package org.vegas.compiler.stringReplace

import org.vegas.compiler.Compiler
import scala.util.matching.Regex

class StringUnloader extends Compiler {
    val stringContent = new Regex("""\"STRING_CONTENT_(\d+)\"""", "index")

    def compile(source: String) =
        Some(stringContent replaceAllIn (source, { m =>
            Compiler.stringStorage.dequeue
        }))
}

object StringUnloader {
    def apply() = new StringUnloader()
}
