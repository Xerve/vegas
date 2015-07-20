package org.vegas.compiler

import java.io.{PrintWriter, File}

trait CompilerWriter { self: Compiler with CompilerReader =>
    val extension: String
    def compileToFile {
        compile(source) match {
            case Some(result) =>
                val writer = new PrintWriter(new File(filename + "." + extension))
                writer.write(result)
                writer.close()
            case None => None
        }
    }
}
