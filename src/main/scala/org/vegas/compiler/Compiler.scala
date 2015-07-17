package org.vegas.compiler

import java.io.{PrintWriter, File}
import scala.collection.mutable.Stack
import org.vegas.vegasc
import org.vegas.vtype.Scope

abstract class Compiler {
    val options = vegasc.options
    def compile(source: String): Option[String]

    def >>:(that: String) = compile(that)
    def >>:(that: FileReader) = compile(that.source)
    def >>:(that: Compiler) = new CompilerPipeline(that, this)
}

object Compiler {
    var refCount = 0;
    val scope = Stack[String]()
    val types = Scope()
    val stringStorage = Stack[String]()

    def usesRef(f: String => String) = {
        refCount += 1
        f("$ref" + refCount)
    }
}

sealed class CompilerPipeline(stage1: Compiler, stage2: Compiler) extends Compiler {
    def compile(source: String) = stage1.compile(source).fold[Option[String]](None)(stage2.compile(_))
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
