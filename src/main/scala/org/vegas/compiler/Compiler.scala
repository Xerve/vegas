package org.vegas.compiler

import java.io.{PrintWriter, File}
import scala.collection.mutable.Queue
import org.vegas.vegasc
import org.vegas.vtype.Scope

abstract class Compiler {
    val options = vegasc.options
    def compile(source: String): Option[String]

    def >>:(that: String) = compile(that)
    def >>:(that: FileReader) = compile(that.fileSource)
    def >>:(that: Compiler) = new CompilerPipeline(that, this)
}

object Compiler {
    var refCount = 0;
    val scope = Scope()
    val stringStorage = Queue[String]()

    def usesRef = {
        refCount += 1
        "$__ref" + refCount
    }

    def usesFunctionRef = {
        refCount += 1
        "__ref" + refCount
    }
}

trait StaticCompiler {
    val compiler: Compiler
    def apply() = compiler
    def apply(source: String) = compiler.compile(source) getOrElse ""
}

sealed class CompilerPipeline(stage1: Compiler, stage2: Compiler) extends Compiler {
    def compile(source: String) = stage1.compile(source).fold[Option[String]](None)(stage2.compile(_))
}

case class FileReader(val filename: String) extends Compiler {
    lazy val file = io.Source.fromFile(filename)
    lazy val fileSource = try file.mkString finally file.close()
    def compile(source: String) = Some(fileSource)
}


case class FileWriter(val filename: String) extends Compiler {
    def compile(source: String) = {
        val writer = new PrintWriter(new File(filename + ".php"))
        writer.write(source)
        writer.close()
        Some("Success!")
    }
}

object PassThru extends Compiler {
    def compile(source: String) = Some(source)
}

object StdOut extends Compiler {
    def compile(source: String) = {
        println(source)
        Some("Success!")
    }
}
