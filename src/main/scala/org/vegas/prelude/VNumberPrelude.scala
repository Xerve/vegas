package org.vegas.prelude

import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VNumberPrelude extends Prelude {
    object VNumberPlus extends VMacro("+") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VNumber)) {
                GenericExpression(VNumber, callee.eval + " ++ " + args.map(_.eval).mkString(" + "))
            }
    }

    def init {
        VNumber define VNumberPlus
    }
}
