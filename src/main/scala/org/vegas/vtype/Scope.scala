package org.vegas.vtype

import org.vegas.log
import scala.collection.mutable.Map

sealed case class Node(val vtype: Option[VType], val mut: Boolean)

class Scope {
    val nodes: Map[String, Node] = Map()

    def add(name: String, vtype: Option[VType], mut: Boolean = false) {
        nodes get name match {
            case Some(node) if !node.mut && !node.vtype.isEmpty => log("Trying to reassign immutable!")
            case Some(node) if !node.mut && node.vtype.isEmpty => nodes(name) = Node(vtype, node.mut)
            case Some(node) if node.mut && !node.vtype.isEmpty => if (isCompatible(node, vtype)) log("Ayy its okay") else log("Not compatible types!")
            case None => nodes += name -> Node(vtype, mut)
        }
    }

    def isCompatible(node: Node, vtype: Option[VType]): Boolean = vtype match {
        case None => true
        case Some(otherType) => node.vtype match {
            case None => true
            case Some(nodeType) if nodeType == otherType => true
            case Some(nodeType) => nodeType.parent match {
                case None => false
                case Some(parent) => isCompatible(node, nodeType.parent)
            }
        }
    }


}

object Scope {
    def apply() = new Scope()
}
