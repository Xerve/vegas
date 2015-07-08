package org.vegas.prelude

import org.vegas.vtype._

package object VAnyPrelude extends Prelude {
    def init {
        object VAnyPrint extends VMacro {
            val name = "print"
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) =
                ast.GenericExpression(VAny, "echo " + callee.eval)
        }

        object VAnyAccessor extends VMacro {
            val name = "."
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) = {
                args.headOption match {
                    case Some(head: ast.IdentifierLiteral) => ast.GenericExpression(VAny, callee.eval + "->" + head.identifier)
                    case _ => new ast.NullExpression()
                }
            }
        }

        object VAnyPlus extends VMacro {
            val name = "+"
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) =
                ast.GenericExpression(VAny, "(" + callee.eval + " + " + args.map(_.eval).mkString(" + ") + ")")
        }

        object VAnyMinus extends VMacro {
            val name = "-"
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) =
                ast.GenericExpression(VAny, "(" + callee.eval + " - " + args.map(_.eval).mkString(" + ") + ")")
        }

        VAny define VAnyPrint
        VAny define VAnyAccessor
        VAny define VAnyPlus
        VAny define VAnyMinus
    }
}
