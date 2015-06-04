package org.vegas.compiler.scope

import scala.collection.immutable.{Seq, Map}

case class ScopeNode(val children: Seq[ScopeNode], val nodes: Map[String, String]) {
    def apply(node: Seq[String]): Option[String] =
        nodes.get(node.head) match {
            case Some(t) => Some(t)
            case None => children.map(_(node.tail)).reduce(_ orElse _)
        }
}
