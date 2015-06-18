package org.vegas.vtype

import org.parboiled2.Parser
import org.vegas.log
import scala.collection.mutable.Map

sealed case class Node(val vtype: Option[VType], val mut: Boolean)

class Scope {
    val nodes: Map[String, Node] = Map()

    def add(name: String, vtype: Option[VType], mut: Boolean = false) {
        nodes get name match {
            case Some(node) if node.mut && !node.vtype.isEmpty => if (isCompatible(node.vtype, vtype)) log("ASSIGN w/ compat!", name) else log("BAD COMPAT", name)
            case Some(node) if !node.mut && !node.vtype.isEmpty => log("CANNOT REASSIGN IMMUTABLE VARIABLE", name)
            case Some(node) if node.vtype.isEmpty => nodes(name) = Node(vtype, node.mut)
            case None => nodes += name -> Node(vtype, mut)
        }
    }

    def isCompatible(node: Option[VType], _vtype: Option[VType]): Boolean = _vtype match {
        case None => true
        case Some(vtype) => node match {
            case None => false
            case Some(nodeType) => if (nodeType == vtype) true else isCompatible(nodeType.parent, Some(vtype))
        }
    }
}

object Scope {
    def apply() = new Scope()
}
