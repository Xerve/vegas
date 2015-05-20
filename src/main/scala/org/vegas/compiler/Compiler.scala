package org.vegas.compiler

import java.io.{PrintWriter, File}

abstract class Compiler {
    def compile(source: String): Option[String]

    def >>:(that: String) = compile(that)
    def >>:(that: FileReader) = compile(that.source)
    def >>:(that: Compiler) = new CompilerPipeline(that, this)
}

sealed class CompilerPipeline(stage1: Compiler, stage2: Compiler) extends Compiler {
    def compile(source: String) = stage1.compile(source) match {
        case Some(result) => stage2.compile(result)
        case None => None
    }
}

case class FileReader(val filename: String) extends Compiler
                                            with CompilerReader {
    def compile(source: String) = Some(source)
}


case class FileWriter(val filename: String) extends Compiler {
    def compile(source: String) = {
        val writer = new PrintWriter(new File(filename + ".php"))
        writer.write("<?php\n" + source)
        writer.close()
        Some("Success!")
    }
}

class PassThru extends Compiler {
    def compile(source: String) = Some(source)
}

object PassThru {
    def apply() = new PassThru()
}
