package org.vegas.compiler.stageN

import org.parboiled2._
import scala.collection.immutable.Seq

package object ast {
    var refCount = 0;

    def usesRef(f: => String) = {
        refCount += 1
        f
    }

    def isMacro(function: String) = false

    abstract class Expression {
        def eval: String
    }

    case class FunctionCall(val function: String, val args: Seq[Expression]) extends Expression {
        def eval = isMacro(function) match {
            case true => "WFTDIDYOUDO"
            case false => function + "(" + args.map(_.eval).mkString(",") + ")"
        }
    }

    case class Binding(val pattern: Pattern, val expression: Expression) extends Expression {
        def eval = pattern decompose expression
    }

    abstract class Pattern {
        def decompose(that: Expression): String
    }

    case class IdentifierPattern(val identifier: String) extends Pattern {
        def decompose(that: Expression) = "$" + identifier + " = " + that.eval
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
        def eval = "array(" + elements.map({ case (key, value) => s"$key => $value" }).mkString(",") + ")"
    }

    case class IdentifierLiteral(val identifier: String) extends Literal {
        def eval = "$" + identifier
    }
}

class StageNParser(val input: ParserInput) extends Parser {
    def Program = rule { Whitespace ~ zeroOrMore(Expression ~ ";" ~ Whitespace) ~ EOI }

    def Expression: Rule1[ast.Expression] = rule {
        FunctionCall |
        Binding |
        Literal
    }

    def Literal = rule {
        IdentifierLiteral ~> (ast.IdentifierLiteral(_)) |
        NumberLiteral ~> (ast.NumberLiteral(_)) |
        StringLiteral ~> (ast.StringLiteral(_)) |
        BooleanLiteral ~> (ast.BooleanLiteral(_)) |
        NullLiteral ~> (ast.NullLiteral(_)) |
        ArrayLiteral ~> (ast.ArrayLiteral(_))
    }

    def Keyword = rule {
        NullKeyword |
        BooleanKeyword
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

    def IdentifierLiteral = rule { capture(Identifier) ~> (_.toString) }

    def Binding = rule { "let" ~ Whitespace ~ Pattern ~ Whitespace ~ "=" ~ Whitespace ~ Expression ~> (ast.Binding(_, _)) }

    def Pattern = rule { IdentifierPattern ~> (ast.IdentifierPattern(_)) |
                         ArrayPattern ~> (ast.ArrayPattern(_)) }

    def IdentifierPattern = rule { capture(Identifier) ~> (_.toString) }

    def ArrayPattern = rule { "[" ~ (capture(Identifier) + ("," ~ Whitespace)) ~ "]" ~> (_.map(_.toString)) }

    def FunctionCall = rule { Function ~ '(' ~ (Expression + (',' ~ Whitespace)) ~ Whitespace ~ ')' ~> (ast.FunctionCall(_, _)) }

    def Function = rule { IdentifierLiteral }
}
