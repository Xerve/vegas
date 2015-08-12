package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression, NullExpression}

package object VCommentPrelude {
    object VCommentComment extends VMacro("/") {
        def eval(callee: Expression, args: Seq[Expression]) =
            require(args, Seq(VString)) {
                GenericExpression(VComment, "// " + args.headOption
                                                        .getOrElse(new NullExpression())
                                                        .eval.stripPrefix("\"")
                                                        .stripSuffix("\"")
                )
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
            args match {
                case Seq(commentBody) if commentBody.vtype != VNull =>
                    GenericExpression(VComment, " * " + args.headOption
                                                            .getOrElse(new NullExpression())
                                                            .eval.stripPrefix("\"")
                                                            .stripSuffix("\"")
                    )
                case Seq(commentBody) =>
                    GenericExpression(VComment, " */")
                case _ =>
                    GenericExpression(VComment, "")
            }
    }

    def init {
        Compiler.scope.add("$/", Some(VComment))

        VComment define VCommentComment
        VComment define VCommentDocblocStart
        VComment define VCommentDocblocBody
    }
}
