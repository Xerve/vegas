package org.vegas.compiler.stageN

import org.vegas.vtype.ast
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

object hint {
    def apply(name: String, args: Seq[String]) =
        name match {
            case "scope" => scope(args)
            case "endscope" => endscope(args)
            case _ => new ast.NullExpression()
        }

    def scope(args: Seq[String]) = {
        args.headOption match {
            case Some(arg) => ast.scope push arg
            case None => None
        }

        new ast.NullExpression()
    }

    def endscope(args: Seq[String]) = {
        ast.scope.pop
        new ast.NullExpression()
    }
}
