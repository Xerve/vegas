package org.vegas.compiler.stage2

import org.vegas.compiler.{Compiler, FileCompiler}
import org.parboiled2.ParseError
import scala.util.{Success, Failure}

class Stage2Compiler extends Compiler {
    def compile(source: String) =
        new Stage2Parser(source).Program.run() match {
            case Success(result) => Some(result.foldLeft("") { (body, expression) => body + expression.eval + ";" })
            case Failure(error: ParseError) =>
                println("Compilation failed with error:\n" + error.format(source))
                None
            case Failure(error) =>
                println("Unexpected error" + error.getMessage)
                None
        }
}

object Stage2Compiler {
    implicit lazy val compiler = new Stage2Compiler()
    def apply() = compiler
}

case class Stage2FileCompiler(val filename: String)
    extends FileCompiler[Stage2Compiler]("stage2")
