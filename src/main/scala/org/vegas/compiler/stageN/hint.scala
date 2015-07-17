package org.vegas.compiler.stageN

import org.vegas.vtype.ast.NullExpression
import org.vegas.compiler.Compiler
import scala.collection.immutable.Seq
import scala.collection.mutable.Stack

object hint {
    def apply(name: String, args: Seq[String]) =
        name match {
            case "scope" => scope(args)
            case "endscope" => endscope(args)
            case _ => new NullExpression()
        }

    def scope(args: Seq[String]) = {
        args.headOption match {
            case Some(arg) => Compiler.scope push arg
            case None => None
        }

        new NullExpression()
    }

    def endscope(args: Seq[String]) = {
        Compiler.scope.pop
        new NullExpression()
    }
}
