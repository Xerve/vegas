package org.vegas.prelude

import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression, NullExpression}

package object VAnyPrelude extends Prelude {
    object VAnyPrint extends VMacro("print") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, "echo " + callee.eval)
    }

    object VAnyAccessor extends VMacro(".") {
        def eval(callee: Expression, args: Seq[Expression]) = {
            args.headOption match {
                case Some(head: ast.IdentifierLiteral) => GenericExpression(VAny, callee.eval + "->" + head.identifier)
                case _ => new NullExpression()
            }
        }
    }

    object VAnyApply extends VMacro("$apply") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, callee.eval + "(" + args.map(_.eval).mkString(",") + ")")
    }

    object VAnyPlus extends VMacro("+") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, callee.eval + " + " + args.map(_.eval).mkString(" + "))
    }

    object VAnyMinus extends VMacro("-") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, callee.eval + " - " + args.map(_.eval).mkString(" - "))
    }

    object VAnyMultiply extends VMacro("*") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, callee.eval + " * " + args.map(_.eval).mkString(" * "))
    }

    object VAnyDivide extends VMacro("/") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VAny, "(" + callee.eval + " / " + args.map(_.eval).mkString(" / ") + ")")
    }

    object VAnyType extends VMacro("$type") {
        def eval(callee: Expression, args: Seq[Expression]) =
            GenericExpression(VString, callee.vtype.toString)
    }

    def init {
        VAny define VAnyPrint
        VAny define VAnyAccessor
        VAny define VAnyApply
        VAny define VAnyPlus
        VAny define VAnyMinus
        VAny define VAnyMultiply
        VAny define VAnyDivide
        VAny define VAnyType
    }
}
