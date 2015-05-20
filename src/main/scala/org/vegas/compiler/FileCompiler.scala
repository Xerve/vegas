package org.vegas.compiler

abstract class FileCompiler[T <: Compiler](val extension: String)
                                          (implicit val compiler: T)
                                          extends Compiler
                                          with CompilerReader
                                          with CompilerWriter {
    def compile(_source: String = source) = compiler.compile(_source)
}
