package org.vegas.vtype

import org.parboiled2.Parser
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

package object ast {
    var refCount = 0;
    val scope = Stack[String]()
    val types = Scope()

    def usesRef(f: String => String) = {
        refCount += 1
        f("$ref" + refCount)
    }

    abstract class Expression {
        def eval: String
        val vtype: VType
    }

    class NullExpression extends Expression {
        def eval = ""
        val vtype = VNull
    }

    case class FunctionChain(val functions: Seq[FunctionCall], ending: Option[IdentifierLiteral]) extends Expression {
        //def eval = functions.map(_.eval).mkString("->") + ending.map("->" + _.identifier).getOrElse("")
        def eval = (functions.tail.foldLeft[Function](functions.head) { (fn, next) =>
            println(fn)
            fn eval next
        }).eval

        val vtype = VAny
    }

    abstract class Function extends Expression {
        def eval(callee: Expression): Function
    }

    case class FunctionCall(val function: IdentifierLiteral, val args: Seq[Expression]) extends Function {
        def eval = function.identifier + "(" + args.map(_.eval).mkString(",") + ")"

        def eval(callee: Expression) = callee.vtype(function.identifier, callee, args).getOrElse(MacroExpression(eval, VAny))

        val vtype = VAny
    }

    case class MacroExpression(val result: String, val vtype: VType) extends Function {
        def eval = result
        def eval(callee: Expression) = MacroExpression(result + callee.eval, callee.vtype)
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
        types.add((scope :+ eval).mkString("\\"), t.map(VType getType _), false, mut)
        def eval = identifier.eval
        def decompose(that: Expression) = {
            types.add((scope :+ eval).mkString("\\"), Some(that.vtype), true)
            identifier.eval + " = " + that.eval
        }

    }

    case class ArrayPattern(val identifiers: Seq[IdentifierPattern]) extends Pattern {
        def decompose(that: Expression) = usesRef { ref =>
            ref + " = " + that.eval + ";" +
            identifiers.zipWithIndex.map {
                case (identifier, index) => identifier.eval + " = " + ref + "[" + index + "];"
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

    case class IdentifierLiteral(val identifiers: Seq[String]) extends Literal {
        def eval = "$" + identifier
        val identifier = identifiers.mkString("->")
        val vtype = VAny
    }

    // def implicitCallToFunction(identifiers: Seq[IdentifierLiteral], args: Seq[Expression]) =
    //     FunctionCall(IdentifierLiteral(identifiers.map(_.eval), args))
    //
    // def implicitCallToFunction(identifiers: Seq[IdentifierLiteral], arg: IdentifierLiteral) =
    //     FunctionCall(IdentifierLiteral(identifiers.map(_.eval), Seq(arg)))
}
