package org.vegas.compiler.stageN

import org.vegas.types.VType
import scala.collection.mutable.Map

sealed case class Node(val vtype: VType, val mut: Boolean)

class ScopeTree {
    val nodes: Map[String, Node] = Map()

    def add(node: String, vtype: VType, mut: Boolean = false) {
        nodes get node match {
            case Some(vnode) =>
                if (!vnode.mut) println("TRYING TO REASSAIGN IMMUTABLE VAR")
                if (vnode.vtype != vtype) println("TRYING TO ASSAIGN WITHOUT MATCHING TYPES")
            case None => nodes += node -> Node(vtype, mut)
        }
    }

    def apply(node: String) = nodes get node
}

object ScopeTree {
    def apply() = new ScopeTree()
}
