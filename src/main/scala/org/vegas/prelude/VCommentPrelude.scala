package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VCommentPrelude {
    object VCommentComment extends VMacro("/") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VString)) {
                GenericExpression(VComment, "// " + args.head.eval.stripPrefix("\"").stripSuffix("\""))
            }
    }

    object VCommentDocblocStart extends VMacro("**") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq()) {
                GenericExpression(VComment, "/**")
            }
    }

    object VCommentDocblocBody extends VMacro("*") {
        def eval(callee: Expression, args: Seq[Expression]) =
            if (args.head.vtype == VNull) {
                GenericExpression(VComment, " */")
            } else {
                GenericExpression(VComment, " * " + args.head.eval.stripPrefix("\"").stripSuffix("\""))
            }
    }

    def init {
        Compiler.scope.add("$/", Some(VComment))


        VComment define VCommentComment
        VComment define VCommentDocblocStart
        VComment define VCommentDocblocBody
    }
}
