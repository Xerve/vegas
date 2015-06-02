package org.vegas.compiler.scope

import org.vegas.compiler.{Compiler, FileCompiler}

class ScopeCompiler extends Compiler {
    def compile(source: String) =
        Some(source)
}

object ScopeCompiler {
    implicit lazy val compiler = new ScopeCompiler()
    def apply() = compiler
}

case class ScopeFileCompiler(val filename: String)
    extends FileCompiler[ScopeCompiler]("scope")
