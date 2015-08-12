package org.vegas.compiler

import org.vegas.vtype.{ast, VComment}
import org.parboiled2.ParseError
import scala.util.{Success, Failure}

class StageNCompiler extends Compiler {
    def compile(source: String) =
        new StageNParser(source).Program.run() match {
            case Success(result) =>
                Some("<?php\n" + StageNCompiler.joinExpressions(result))
            case Failure(error: ParseError) =>
                println("Compilation failed with error:\n" + error.format(source))
                None
            case Failure(error) =>
                println("Unexpected error" + error.getMessage)
                None
            case _ => None
        }
}

object StageNCompiler extends StaticCompiler {
    lazy val compiler = new StageNCompiler()

    def joinExpressions(expressions: Seq[ast.Expression]) =
        expressions.foldLeft("") { (body, expression) =>
            val result = expression.eval
            body + result + (if (expression.vtype != VComment && !result.trim.endsWith("}")) ";\n" else "\n")
        }
}
