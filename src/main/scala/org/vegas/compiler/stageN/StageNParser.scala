package org.vegas.compiler.stageN

import org.parboiled2._
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

object ast {
    var refCount = 0;
    var scope = Stack[String]()

    def usesRef(f: => String) = {
        refCount += 1
        f
    }

    abstract class Expression {
        def eval: String
    }

    object NullExpression extends Expression {
        def eval = ""
    }

    case class FunctionCall(val function: IdentifierLiteral, val args: Seq[Expression]) extends Expression {
        def eval = function.eval + "(" + args.map(_.eval).mkString(",") + ")"
    }

    case class Binding(val pattern: Pattern, val expression: Expression) extends Expression {
        def eval = pattern decompose expression
    }

    abstract class Pattern {
        def decompose(that: Expression): String
    }

    case class IdentifierPattern(val identifier: IdentifierLiteral) extends Pattern {
        def decompose(that: Expression) = identifier.eval + " = " + that.eval
    }

    case class ArrayPattern(val identifiers: Seq[String]) extends Pattern {
        def decompose(that: Expression) = usesRef {
            "$ref" + refCount + " = " + that.eval + ";" +
            identifiers.zipWithIndex.map {
                case (identifier, index) => "$" + identifier + " = $ref" + refCount + "[" + index + "];"
            }.mkString.init
        }
    }

    abstract class Literal extends Expression

    case class NumberLiteral(val value: String) extends Literal {
        def eval = value
    }

    case class StringLiteral(val value: String) extends Literal {
        def eval = "\"" + value + "\""
    }

    case class BooleanLiteral(val value: String) extends Literal {
        def eval = value match {
            case "true" | "yes" | "on" => "true"
            case "false" | "no" | "off" => "false"
            case _ => "null"
        }
    }

    case class NullLiteral(val value: String) extends Literal {
        def eval = "null"
    }

    case class ArrayLiteral(val elements: Seq[Expression]) extends Literal {
        def eval = "array(" + elements.map(_.eval).mkString(",") + ")"
    }

    case class ObjectLiteral(val elements: Seq[Tuple2[String, Expression]]) extends Literal {
        def eval = "array(" + elements.map({ case (key, value) => "\"" + key + "\" => " + value.eval }).mkString(",") + ")"
    }

    case class IdentifierLiteral(val identifier: String) extends Literal {
        def eval = "$" + identifier.replaceAllLiterally(".", "->")
    }

    def implicitCallToFunction(identifiers: Seq[IdentifierLiteral], args: Seq[Expression]) =
        FunctionCall(IdentifierLiteral(identifiers.map(_.eval.tail).mkString(".")), args)

    def implicitCallToFunction(identifiers: Seq[IdentifierLiteral], arg: IdentifierLiteral) =
        FunctionCall(IdentifierLiteral(identifiers.map(_.eval.tail).mkString(".")), Seq(arg))
}

object hint {
    def apply(name: String, args: Seq[String]) =
        name match {
            case "scope" => scope(args)
            case "endscope" => endscope(args)
            case _ => ast.NullExpression
        }

    def scope(args: Seq[String]) = {
        args.headOption match {
            case Some(arg) => ast.scope push arg
            case None => None
        }

        ast.NullExpression
    }

    def endscope(args: Seq[String]) = {
        ast.scope.pop
        ast.NullExpression
    }
}

class StageNParser(val input: ParserInput) extends Parser {
    def Program = rule { Whitespace ~ zeroOrMore(Expression ~ ";" ~ Whitespace) ~ EOI }

    def Expression: Rule1[ast.Expression] = rule {
        CompilerHint |
        FunctionCall |
        Binding |
        Literal |
        '(' ~ Whitespace ~ Expression ~ Whitespace ~ ')'
    }

    def Literal = rule {
        IdentifierLiteral |
        NumberLiteral ~> (ast.NumberLiteral(_)) |
        StringLiteral ~> (ast.StringLiteral(_)) |
        BooleanLiteral ~> (ast.BooleanLiteral(_)) |
        NullLiteral ~> (ast.NullLiteral(_)) |
        ArrayLiteral ~> (ast.ArrayLiteral(_)) |
        ObjectLiteral ~> (ast.ObjectLiteral(_))
    }

    def Keyword = rule {
        NullKeyword |
        BooleanKeyword |
        "let"
    }

    def Identifier = rule {
        !(ch('"') ~ ANY) ~
        !Keyword ~
        CharPredicate("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") ~
        zeroOrMore(CharPredicate("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    }

    def Whitespace = rule { zeroOrMore(CharPredicate(" \n\r\t\f")) }

    def NumberLiteral = rule { capture(Digits) ~> (_.toString) }

    def Digits = rule { optional(ch('+') | '-') ~ oneOrMore(CharPredicate.Digit) ~ optional('.') ~ zeroOrMore(CharPredicate.Digit) }

    def StringLiteral = rule { '"' ~ capture(StringChars) ~ '"' ~> (_.toString) }

    def StringChars = rule { zeroOrMore(!ch('"') ~ (atomic("\\\"") | ANY)) }

    def NullLiteral = rule { capture(NullKeyword) ~> (_.toString) }

    def NullKeyword = rule { "null" }

    def BooleanLiteral = rule { capture(BooleanKeyword) ~> (_.toString) }

    def BooleanKeyword = rule { "true" | "yes" | "on" | "false" | "no" | "off" }

    def ArrayLiteral = rule { '[' ~ Whitespace ~ (Expression + (',' ~ Whitespace)) ~ Whitespace ~ ']' }

    def ObjectLiteral = rule { '{' ~ Whitespace ~ (ObjectAttribute + (',' ~ Whitespace)) ~ Whitespace ~ '}' }

    def ObjectAttribute = rule { (capture(Identifier) | StringLiteral) ~ Whitespace ~ ':' ~ Whitespace ~ Expression ~> (Tuple2(_, _)) }

    def IdentifierLiteral = rule { capture(Identifier) ~> (ast.IdentifierLiteral(_)) }

    def Binding = rule { "let" ~ Whitespace ~ Pattern ~ Whitespace ~ "=" ~ Whitespace ~ Expression ~> (ast.Binding(_, _)) }

    def Pattern = rule {
        IdentifierPattern ~> (ast.IdentifierPattern(_)) |
        ArrayPattern ~> (ast.ArrayPattern(_))
    }

    def IdentifierPattern = rule { IdentifierLiteral }

    def ArrayPattern = rule { "[" ~ (capture(Identifier) + ("," ~ Whitespace)) ~ "]" ~> (_.map(_.toString)) }

    def FunctionCall = rule {
        ImplicitFunctionCall |
        ExplicitFunctionCall
    }

    def ExplicitFunctionCall = rule { IdentifierLiteral ~ '(' ~ (Expression * (',' ~ Whitespace)) ~ Whitespace ~ ')' ~> (ast.FunctionCall(_, _)) }

    def ImplicitFunctionCall = rule {
        oneOrMore(IdentifierLiteral ~ ' ') ~ (Expression + (',' ~ Whitespace)) ~> (ast.implicitCallToFunction(_, _))
    }

    def CompilerHint = rule { "#[" ~ capture(oneOrMore(CharPredicate.AlphaNum)) ~ ']' ~ Whitespace ~ (('<' ~ capture(oneOrMore(CharPredicate.AlphaNum)) ~ '>') * Whitespace) ~> (hint(_, _)) }
}
