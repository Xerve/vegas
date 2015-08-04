package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VegasPrelude extends Prelude {
    object VegasNormalize extends VMacro("normalize") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VAny)) {
                GenericExpression(args.head.vtype, args.head.eval)
            }
    }

    def init {
        Compiler.scope.add("$vegas", Some(Vegas))
        Vegas define VegasNormalize
    }
}
