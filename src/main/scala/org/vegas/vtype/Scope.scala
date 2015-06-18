package org.vegas.vtype

import org.parboiled2.Parser
import org.vegas.log
import scala.collection.mutable.Map

sealed case class Node(val vtype: Option[VType], val mut: Boolean, val assigned: Boolean)

class Scope {
    val nodes: Map[String, Node] = Map()

    def add(name: String, vtype: Option[VType], assign: Boolean = false, mut: Boolean = false) {
        nodes get name match {
            case Some(node) if node.mut && !node.vtype.isEmpty && !isCompatible(node.vtype, vtype) =>
                log(s"Trying to assign $name with incompatible types", s"${node.vtype} to $vtype")

            case Some(node) if !node.mut && !node.vtype.isEmpty && node.assigned =>
                log(s"Trying to reassign immutable variable $name")

            case Some(node) =>
                if (node.vtype.isEmpty) nodes(name) = Node(vtype, node.mut, assign)
                if (!node.assigned) nodes(name) = Node(node.vtype orElse vtype, node.mut, assign)

            case None => nodes += name -> Node(vtype, mut, assign)
            case _ => Unit
        }
    }

    def isCompatible(node: Option[VType], _vtype: Option[VType]): Boolean = _vtype match {
        case None => true
        case Some(vtype) => node match {
            case None => false
            case Some(nodeType) => if (nodeType == vtype) true else isCompatible(nodeType.parent, Some(vtype))
        }
    }

    override def toString =
        "Scope:\n" +
        nodes.toSeq.map({ case (name: String, node: Node) =>
            name + "[" +
            node.vtype.map(_.typename).getOrElse("__None__") +
            "]" +
            (if (node.mut) "*" else "")
        }).mkString("\n")
}

object Scope {
    def apply() = new Scope()
}
