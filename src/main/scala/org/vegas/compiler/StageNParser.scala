package org.vegas.compiler

import org.vegas.vtype.ast
import org.parboiled2._
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

class StageNParser(val input: ParserInput) extends Parser {
    def Program = rule { Whitespace ~ oneOrMore(Expression ~ ";" ~ Whitespace) ~ EOI }

    def Expression: Rule1[ast.Expression] = rule {
        ParenExpression |
        FunctionCall |
        Block |
        Literal |
        Binding
    }

    def ParenExpression = rule {
        quiet('(') ~ Whitespace ~ Expression ~ Whitespace ~ quiet(')')
    }

    def Literal = rule {
        IdentifierLiteral |
        NumberLiteral ~> (ast.NumberLiteral(_)) |
        StringLiteral ~> (ast.StringLiteral(_)) |
        NullLiteral ~> (ast.NullLiteral(_)) |
        BooleanLiteral ~> (ast.BooleanLiteral(_)) |
        ObjectLiteral ~> (ast.ObjectLiteral(_)) |
        ArrayLiteral ~> (ast.ArrayLiteral(_))
    }

    def Keyword = rule {
        NullKeyword |
        BooleanKeyword |
        atomic("let")
    }

    def Identifier = rule {
        !(ch('"') ~ ANY) ~
        !Keyword ~
        (OperatorCharacter |
        (CharPredicate("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_") ~
        zeroOrMore(CharPredicate("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))))
    }

    def FunctionName = rule {
        oneOrMore(CharPredicate("$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))
    }

    def OperatorCharacter = rule {
        CharPredicate(".<>*&%+-|?:/")
    }

    def OperatorName = rule {
        oneOrMore(OperatorCharacter)
    }

    def Whitespace = rule { quiet(zeroOrMore(CharPredicate(" \n\r\t\f"))) }

    def NumberLiteral = rule { capture(Digits) ~> (_.toString) }

    def Digits = rule { optional(ch('+') | '-') ~ oneOrMore(CharPredicate.Digit) ~ optional('.') ~ zeroOrMore(CharPredicate.Digit) }

    def StringLiteral = rule { '"' ~ capture(StringChars) ~ '"' ~> (_.toString) }

    def StringChars = rule { zeroOrMore(!ch('"') ~ (atomic("\\\"") | ANY)) }

    def NullLiteral = rule { capture(NullKeyword) ~> (_.toString) }

    def NullKeyword = rule { atomic("null") }

    def BooleanLiteral = rule { capture(BooleanKeyword) ~> (_.toString) }

    def BooleanKeyword = rule { atomic("true") | atomic("yes") | atomic("on") | atomic("false") | atomic("no") | atomic("off") }

    def ArrayLiteral = rule { '[' ~ Whitespace ~ (Expression + (',' ~ Whitespace)) ~ Whitespace ~ ';' ~ Whitespace ~ ']' }

    def ObjectLiteral = rule { '[' ~ Whitespace ~ (ObjectAttribute + (',' ~ Whitespace)) ~ Whitespace ~ ';' ~ Whitespace ~ ']' }

    def ObjectAttribute = rule { (capture(Identifier) | StringLiteral) ~ Whitespace ~ atomic("=>") ~ Whitespace ~ Expression ~> (Tuple2(_, _)) }

    def IdentifierLiteral = rule { capture(Identifier) ~ '!' ~> (ast.IdentifierLiteral(_, nonVariable = true)) |
                                   capture(Identifier) ~> (ast.IdentifierLiteral(_, nonVariable = false))}

    def Binding = rule { atomic("let") ~ Whitespace ~ Pattern ~ Whitespace ~ "=" ~ Whitespace ~ Expression ~> (ast.Binding(_, _)) }

    def Pattern = rule {
        IdentifierPattern |
        ArrayPattern
    }

    def IdentifierPattern = rule {
        atomic("mut") ~ Whitespace ~ IdentifierLiteral ~ optional(":" ~ Whitespace ~ capture(Identifier)) ~> (ast.IdentifierPattern(_, _, true)) |
        IdentifierLiteral ~ optional(":" ~ Whitespace ~ capture(Identifier)) ~> (ast.IdentifierPattern(_, _, false))
    }

    def ArrayPattern = rule { "[" ~ (IdentifierPattern + ("," ~ Whitespace)) ~ "]" ~> (ast.ArrayPattern(_)) }

    def FunctionCall: Rule1[ast.FunctionCall] = rule {
        Literal ~ Whitespace ~ (FunctionCaller + Whitespace) ~> (ast.FunctionCall(_, _))
    }

    def FunctionCaller: Rule1[ast.FunctionCaller] = rule {
        '(' ~ Expression ~ ')' ~> (ast.FunctionCaller("$apply", _)) |
        (capture(OperatorName) ~ Whitespace ~ Literal ~> (ast.FunctionCaller(_, _))) |
        (capture(OperatorName) ~ Whitespace ~ Expression ~> (ast.FunctionCaller(_, _))) |
        (capture(OperatorName) ~> (ast.FunctionCaller(_, new ast.NullExpression()))) |
        (capture(FunctionName) ~ Whitespace ~ (Expression * (',' ~ Whitespace)) ~> (ast.FunctionCaller(_: String, _: Seq[ast.Expression])))
    }

    def Block = rule { '{' ~ Whitespace ~ zeroOrMore(Expression ~ ';' ~ Whitespace) ~ '}' ~> (ast.Block(_)) }
}
