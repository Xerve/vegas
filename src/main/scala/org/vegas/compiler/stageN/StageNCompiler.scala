package org.vegas.compiler.stageN

import org.vegas.compiler.{Compiler, FileCompiler}
import org.parboiled2.ParseError
import scala.util.{Success, Failure}

class StageNCompiler extends Compiler {
    def compile(source: String) =
        new StageNParser(source).Program.run() match {
            case Success(result) => { println(ast.types.nodes); Some(result.foldLeft("") { (body, expression) => body + expression.eval + ";" }) }
            case Failure(error: ParseError) =>
                println("Compilation failed with error:\n" + error.format(source))
                None
            case Failure(error) =>
                println("Unexpected error" + error.getMessage)
                None
        }
}

object StageNCompiler {
    implicit lazy val compiler = new StageNCompiler()
    def apply() = compiler
}

case class StageNFileCompiler(val filename: String)
    extends FileCompiler[StageNCompiler]("stageN")
