package org.vegas.compiler.scope

import org.vegas.compiler.{Compiler, FileCompiler, StaticCompiler}

class ScopeCompiler extends Compiler {
    def compile(source: String) =
        Some(source)
}

object ScopeCompiler extends StaticCompiler {
    implicit lazy val compiler = new ScopeCompiler()
}

case class ScopeFileCompiler(val filename: String)
    extends FileCompiler[ScopeCompiler]("scope")
