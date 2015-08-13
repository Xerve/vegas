package org.vegas.vtype.ast

import org.parboiled2.Parser
import org.vegas.compiler.{Compiler, StageNCompiler}
import org.vegas.vtype._
import scala.collection.immutable.Seq

abstract class Expression {
    def eval: String
    val vtype: VType

    def callMacro(macroName: String, args: Seq[Expression]) = vtype.call(macroName, this, args)
}

case class GenericExpression(val vtype: VType, val result: String) extends Expression {
    def eval = result
}

class NullExpression extends Expression {
    def eval = "null"
    val vtype = VNull
}

class FunctionCaller(val functionName: String, val args: Seq[Expression])

object FunctionCaller {
    def apply(functionName: String, args: Seq[Expression]) = new FunctionCaller(functionName, args)
    def apply(functionName: String, arg: Expression) = new FunctionCaller(functionName, Seq(arg))
}

case class FunctionCall(val callee: Expression, val functions: Seq[FunctionCaller]) extends Expression {
    lazy val result =
        (functions.foldLeft(callee) { (expression, caller) =>
            expression.callMacro(caller.functionName, caller.args) match {
                case Some(result) => result
                case None => GenericExpression(VAny, expression.eval +
                    (if (caller.functionName != "") "->" else "") +
                    caller.functionName +
                    "(" + caller.args.map(_.eval).mkString(", ") + ")"
                )
            }
        })

    lazy val vtype = result.vtype
    def eval = result.eval
}

case class Binding(val pattern: Pattern, val expression: Expression) extends Expression {
    def eval = pattern decompose expression
    val vtype = VAny
}

abstract class Pattern extends Expression {
    def decompose(that: Expression): String
    val vtype = VAny
}

case class IdentifierPattern(val identifier: IdentifierLiteral, val t: Option[String], val mut: Boolean) extends Pattern {
    Compiler.scope.add(eval, t.map(VType getType _), false, mut)
    def eval = identifier.eval
    def decompose(that: Expression) = {
        Compiler.scope.add(eval, Some(that.vtype), true)
        identifier.eval + " = " + that.eval
    }

}

case class ArrayPattern(val identifiers: Seq[IdentifierPattern]) extends Pattern {
    def decompose(that: Expression) = {
        val ref = Compiler.usesRef
        ref + " = " + that.eval + ";" +
        identifiers.zipWithIndex.map {
            case (identifier, index) => identifier.eval + " = $" + ref + "[" + index + "];"
        }.mkString.init
    }

    def eval = "[" + identifiers.mkString(", ") + "]"
}

abstract class Literal extends Expression

case class NumberLiteral(val value: String) extends Literal {
    def eval = value
    val vtype = VNumber
}

case class StringLiteral(val value: String) extends Literal {
    def eval = "\"" + value + "\""
    val vtype = VString
}

case class BooleanLiteral(val value: String) extends Literal {
    def eval = value match {
        case "true" | "yes" | "on" => "true"
        case "false" | "no" | "off" => "false"
        case _ => "null"
    }

    val vtype = VBoolean
}

case class NullLiteral(val value: String) extends Literal {
    def eval = "null"
    val vtype = VNull
}

case class ArrayLiteral(val elements: Seq[Expression]) extends Literal {
    def eval = "array(" + elements.map(_.eval).mkString(",") + ")"
    val vtype = VAny
}

case class ObjectLiteral(val elements: Seq[Tuple2[String, Expression]]) extends Literal {
    def eval = "array(" + elements.map({ case (key, value) => "\"" + key + "\" => " + value.eval }).mkString(",") + ")"
    val vtype = VAny
}

case class IdentifierLiteral(val identifier: String, val nonVariable: Boolean = false) extends Literal {
    def eval = (if (nonVariable) "" else "$") + identifier
    lazy val vtype = Compiler.scope(eval).getOrElse(VAny)
}

case class Block(body: Seq[Expression]) extends Expression {
    def eval = "{\n" + StageNCompiler.joinExpressions(body) + "}"
    def simple = StageNCompiler.joinExpressions(body)

    val vtype = VBlock(body.lastOption.map(_.vtype) getOrElse VNull)
}
