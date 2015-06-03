package org.vegas.compiler.scope

import scala.collection.immutable.{Seq, Map}

type Scope = Seq[String]

case class ScopeNode(val children: Seq[ScopeNode], val nodes: Map[String, String]) {
    def apply(node: Scope) =
        nodes.get(node.head) match {
            case Some(t) => t
            case None => children.foldLeft(None) { (prev, scope) =>
                prev match {
                    case Some(t) => prev
                    case None => scope(node.tail)
                }
            }
        }
}
