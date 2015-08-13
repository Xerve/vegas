package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Block, Expression, GenericExpression, NullExpression}

package object VConditionalPrelude {
    object VConditionalApply extends VMacro("$apply") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee.vtype match {
                case VIf =>
                    GenericExpression(VIf, "if (" + args.map(_.eval).mkString + ") ")

                case VElif => //require(args, Seq(VAny)) {
                    GenericExpression(VElif, "elseif (" + args.map(_.eval).mkString + ") ")

                case _ => println(callee.vtype);new NullExpression()
            }
    }

    object VConditionalColon extends VMacro(":") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee.vtype match {
                case VIf =>
                    GenericExpression(VAny, callee.eval + args.headOption.map(_.eval).mkString)

                case VElif => //require(args, Seq(VAny)) {
                    GenericExpression(VAny, callee.eval + args.headOption.map(_.eval).mkString)

                case VElse => //require(args, Seq(VAny)) {
                    GenericExpression(VAny, "else " + args.headOption.map(_.eval).mkString)

                case _ =>
                    new GenericExpression(VAny, callee.eval + args.headOption.map(_.eval).mkString)
            }

    }

    def init {
        Compiler.scope.add("$if", Some(VIf))
        Compiler.scope.add("$elif", Some(VElif))
        Compiler.scope.add("$else", Some(VElse))

        VConditional define VConditionalApply
        VConditional define VConditionalColon
    }
}
