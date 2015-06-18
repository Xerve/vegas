package org.vegas.compiler.stageN

import org.vegas.vtype.ast
import org.parboiled2._
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

class StageNParser(val input: ParserInput) extends Parser {
    implicit val parser = this
    
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

    def Whitespace = rule { quiet(zeroOrMore(CharPredicate(" \n\r\t\f"))) }

    def NumberLiteral = rule { capture(Digits) ~> (_.toString) }

    def Digits = rule { optional(ch('+') | '-') ~ oneOrMore(CharPredicate.Digit) ~ optional('.') ~ zeroOrMore(CharPredicate.Digit) }

    def StringLiteral = rule { '"' ~ capture(StringChars) ~ '"' ~> (_.toString) }

    def StringChars = rule { zeroOrMore(!ch('"') ~ (atomic("\\\"") | ANY)) }

    def NullLiteral = rule { capture(NullKeyword) ~> (_.toString) }

    def NullKeyword = rule { "null" }

    def BooleanLiteral = rule { capture(BooleanKeyword) ~> (_.toString) }

    def BooleanKeyword = rule { "true" | "yes" | "on" | "false" | "no" | "off" }

    def ArrayLiteral = rule { '[' ~ Whitespace ~ (Expression + (',' ~ Whitespace)) ~ Whitespace ~ ']' }

    def ObjectLiteral = rule { '{' ~ Whitespace ~ (ObjectAttribute + (',' ~ Whitespace)) ~ Whitespace ~ ';' ~ Whitespace ~ '}' }

    def ObjectAttribute = rule { (capture(Identifier) | StringLiteral) ~ Whitespace ~ atomic("=>") ~ Whitespace ~ Expression ~> (Tuple2(_, _)) }

    def IdentifierLiteral = rule { capture(Identifier) ~> (ast.IdentifierLiteral(_)) }

    def Binding = rule { "let" ~ Whitespace ~ Pattern ~ Whitespace ~ "=" ~ Whitespace ~ Expression ~> (ast.Binding(_, _)) }

    def Pattern = rule {
        IdentifierPattern |
        ArrayPattern
    }

    def IdentifierPattern = rule {
        "mut" ~ Whitespace ~ IdentifierLiteral ~ optional(":" ~ Whitespace ~ capture(Identifier)) ~> (ast.IdentifierPattern(_, _, true)) |
        IdentifierLiteral ~ optional(":" ~ Whitespace ~ capture(Identifier)) ~> (ast.IdentifierPattern(_, _, false))
    }

    def ArrayPattern = rule { "[" ~ (IdentifierPattern + ("," ~ Whitespace)) ~ "]" ~> (ast.ArrayPattern(_)) }

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
