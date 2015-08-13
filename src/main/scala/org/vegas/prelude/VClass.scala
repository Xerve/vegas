package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression, NullExpression}

package object VClassPrelude extends Prelude {
    object VClassDeclarationApply extends VMacro("$apply") {
        def eval(callee: Expression, args: Seq[Expression]) = {
            val newClass = args.headOption getOrElse new NullExpression()
            val className = newClass match {
                case name: ast.IdentifierLiteral => name.identifier
                case _ => "ClassName"
            }

            GenericExpression(VClassBuilder(className), s"class $className ")
        }
    }

    object VClassDeclarationColon extends VMacro(":") {
        def eval(callee: Expression, args: Seq[Expression]) =
            callee.vtype match {
                case vtype: VClassBuilder =>
                    Compiler.scope.add(vtype.className, Some(VClassParent), true)
                    Compiler.scope push vtype.className
                    val result  = callee.eval + args.map(_.eval).mkString("\n")
                    Compiler.scope.pop
                    GenericExpression(VClass(vtype.className), result)
                case _ =>
                    new NullExpression()
            }
    }

    def init {
        Compiler.scope.add("$class", Some(VClassDeclaration))

        VClassDeclaration define VClassDeclarationApply
        VClassDeclaration define VClassDeclarationColon
    }
}
