package org.vegas.compiler

import org.vegas.vtype.ast
import org.parboiled2.ParseError
import scala.util.{Success, Failure}

class StageNCompiler extends Compiler {
    def compile(source: String) =
        new StageNParser(source).Program.run() match {
            case Success(result) => { Some("<?php\n" + result.foldLeft("") { (body, expression) => body + expression.eval + ";\n" }) }
            case Failure(error: ParseError) =>
                println("Compilation failed with error:\n" + error.format(source))
                None
            case Failure(error) =>
                println("Unexpected error" + error.getMessage)
                None
        }
}

object StageNCompiler extends StaticCompiler {
    lazy val compiler = new StageNCompiler()
}
