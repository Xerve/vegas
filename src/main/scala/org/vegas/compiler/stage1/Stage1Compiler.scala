package org.vegas.compiler.stage1

import org.vegas.compiler.{Compiler, FileCompiler}

class Stage1Compiler extends Compiler {
    def compile(source: String) =
        Some(source)
}

object Stage1Compiler {
    implicit lazy val compiler = new Stage1Compiler()
    def apply() = compiler
}

case class Stage1FileCompiler(val filename: String)
    extends FileCompiler[Stage1Compiler]("stage1")
