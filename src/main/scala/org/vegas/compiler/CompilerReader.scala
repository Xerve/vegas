package org.vegas.compiler

trait CompilerReader {
    self: Compiler =>

    val filename: String
    lazy val file = io.Source.fromFile(filename)
    lazy val source = try file.mkString finally file.close()
}
