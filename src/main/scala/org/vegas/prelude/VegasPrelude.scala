package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VegasPrelude extends Prelude {
    object VegasComment extends VMacro("/") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VString)) {
                GenericExpression(VNull, "// " + args.head.eval.stripPrefix("\"").stripSuffix("\""))
            }
    }

    object VegasDocblocStart extends VMacro("**") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq()) {
                GenericExpression(VNull, "/**")
            }
    }

    object VegasDocblocBody extends VMacro("*") {
        def eval(callee: Expression, args: Seq[Expression]) =
            if (args.head.vtype == VNull) {
                GenericExpression(VNull, " */")
            } else {
                GenericExpression(VNull, " * " + args.head.eval.stripPrefix("\"").stripSuffix("\""))
            }
    }

    def init {
        Compiler.scope.add("$vegas", Some(Vegas))
        Compiler.scope.add("$/", Some(Vegas))

        Vegas define VegasComment
        Vegas define VegasDocblocStart
        Vegas define VegasDocblocBody
    }
}
