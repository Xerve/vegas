package org.vegas.prelude

import org.vegas.vtype._

package object VAnyPrelude extends Prelude {
    def init {
        object VAnyPrint extends VMacro {
            implicit val vtype = VAny
            val name = "print"
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) =
                ast.GenericExpression("echo " + callee.eval)
        }

        object VAnyAccessor extends VMacro {
            val name = "."
            def eval(callee: ast.Expression, args: Seq[ast.Expression]) = {
                args headOption match {
                    case Some(head: ast.IdentifierLiteral) => ast.GenericExpression(callee.eval + "->" + head.identifier)(VAny)
                    case _ => new ast.NullExpression()
                }
            }
        }

        VAny define "print" -> VAnyPrint
        VAny define "." -> VAnyAccessor
    }
}
