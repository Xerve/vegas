package org.vegas.compiler.stageN

import org.vegas.compiler.VType
import scala.collection.mutable.Map

sealed case class Node(val vtype: VType, val mut: Boolean)

class Scope {
    val nodes: Map[String, Node] = Map()

    def add(node: String, value: String, mut: Boolean = false) {
        nodes get node match {
            case Some(vnode) =>
                if (!vnode.mut) println("TRYING TO REASSAIGN IMMUTABLE VAR")
                if (vnode.vtype != VType(value)) println("TRYING TO ASSAIGN WITHOUT MATCHING TYPES")
            case None => nodes += node -> Node(VType getType value, mut)
        }
    }

    def apply(node: String) = nodes get node
}

object Scope {
    def apply() = new Scope()
}
