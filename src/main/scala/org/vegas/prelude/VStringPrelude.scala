package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VStringPrelude extends Prelude {
    object VStringPlus extends VMacro("+") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VString)) {
                GenericExpression(VString, callee.eval + " . " + args.map(_.eval).mkString(" . "))
            }
    }

    object VStringTimes extends VMacro("*") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                val base = callee.eval
                val times = args.headOption.map(_.eval) getOrElse 0
                GenericExpression(VString, s"str_repeat($base, $times)")
            }
    }

    def init {
        VString define VStringPlus
        VString define VStringTimes
    }
}
