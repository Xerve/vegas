package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Block, Expression, GenericExpression, IfExpression, NullExpression}

package object VConditionalPrelude {
    object VConditionalApply extends VMacro("$apply") {
        def eval(callee: Expression, args: Seq[Expression]) =
            new GenericExpression(VIf(), "if (" + args.map(_.eval).mkString + ") ") with IfExpression {
                val statement = args.headOption getOrElse new NullExpression()
                val body = new NullExpression()
            }
    }

    object VConditionalColon extends VMacro(":") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee match {
                case ifExpression: GenericExpression with IfExpression =>
                    new GenericExpression(VIf(VAny, true), callee.eval + args.headOption.map(_.eval).mkString) with IfExpression {
                        val statement = ifExpression.statement
                        val body = args.headOption getOrElse new NullExpression()
                    }
                case _ =>
                    new GenericExpression(VIf(VAny, true), callee.eval + args.headOption.map(_.eval).mkString)
            }

    }

    object VConditionalThen extends VMacro("then") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee match {
                case ifExpression: GenericExpression with IfExpression =>
                    new GenericExpression(VIf(VAny, false), callee.eval + args.headOption.map(_.eval).mkString) with IfExpression {
                        val statement = ifExpression.statement
                        val body = args.headOption getOrElse new NullExpression()
                    }
                case _ =>
                    new GenericExpression(VIf(VAny, false), callee.eval + args.headOption.map(_.eval).mkString)
            }
    }

    object VConditionalElse extends VMacro("else") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee match {
                case ifExpression: GenericExpression with IfExpression =>
                    ifExpression.vtype match {
                        case ifType: VIf if !ifType.structural =>
                            new GenericExpression(ifType.resultType, "(" + ifExpression.statement.eval + " ? " +
                                                                     (ifExpression.body match {
                                                                        case body: Block => body.simple
                                                                        case body => body.eval
                                                                     }) + " : " +
                                                                     (args.headOption match {
                                                                         case Some(elseBody: Block) => elseBody.simple
                                                                         case Some(elseBody) => elseBody.eval
                                                                         case None => new NullExpression()
                                                                     }) + ")")
                        case ifType: VIf =>
                            new GenericExpression(VAny, callee.eval + " else " + args.map(_.eval).mkString)

                        case _ =>
                            println("owww");new NullExpression()
                    }

                case _ =>
                    new NullExpression()
            }
    }

    def init {
        Compiler.scope.add("$if", Some(VConditional))

        VConditional define VConditionalApply
        VConditional define VConditionalColon
        VConditional define VConditionalThen
        VConditional define VConditionalElse
    }
}
