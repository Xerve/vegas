package org.vegas.prelude

import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VNumberPrelude extends Prelude {
    object VNumberPlus extends VMacro("+") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, callee.eval + " + " + args.map(_.eval).mkString(" + "))
            }
    }

    object VNumberMinus extends VMacro("-") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, callee.eval + " - " + args.map(_.eval).mkString(" - "))
            }
    }

    object VNumberMultiply extends VMacro("*") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, callee.eval + " * " + args.map(_.eval).mkString(" * "))
            }
    }

    object VNumberDivide extends VMacro("/") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, "(" + callee.eval + " / " + args.map(_.eval).mkString(" / ") + ")")
            }
    }

    object VNumberPower extends VMacro("**") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, "pow(" + callee.eval + ", " + args.map(_.eval).mkString(", ") + ")")
            }
    }

    def init {
        VNumber define VNumberPlus
        VNumber define VNumberMinus
        VNumber define VNumberMultiply
        VNumber define VNumberDivide
        VNumber define VNumberPower
    }
}
