package org.vegas.vtype

import scala.collection.immutable.Seq
import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

abstract class VType {
    val parent: Option[VType]
    val typename: String
    val macros: Map[String, Seq[ast.Expression] => String] = Map()

    def define(_macro: Tuple2[String, Seq[ast.Expression] => String]) {
        macros += _macro
    }

    def apply(_macro: String, args: Seq[ast.Expression]): Option[String] =
        macros get _macro match {
            case Some(vmacro) => Some(vmacro(args))
            case None => parent match {
                case Some(parent) => parent(_macro, args)
                case None => None
            }
        }

    override def toString = typename
}

object VType {
    def apply(sample: String, types: Option[Seq[String]] = None) = sample match {
        case VNumber(sample) => VNumber
        case VString(sample) => VString
        case VBoolean(sample) => VBoolean
        case _ => VAny
    }

    def getType(vtype: String) = vtype match {
        case "Int" => VNumber
        case "Number" => VNumber
        case "String" => VString
        case "Boolean" => VBoolean
        case _ => VAny
    }
}

trait Generic {
    self: VType =>

    val types: Seq[VType]
}

object VAny extends VType {
    val parent = None
    val typename = "Any"
    def unapply(sample: String) = Some(this)
}

object VNull extends VType {
    val parent = None
    val typename = "Null"
    def unapply(sample: String) = if (sample == "null") Some(this) else None
}

object VNumber extends VType {
    val parent = Some(VAny)
    val typename = "Number"
    def unapply(sample: String) = if (sample matches """^-?\d+\.?$""") Some(this) else None
}

object VString extends VType {
    val parent = Some(VAny)
    val typename = "String"
    def unapply(sample: String) = if (sample matches """"(?:\\.|[^"\\])*""") Some(this) else None
}

object VBoolean extends VType {
    val parent = Some(VAny)
    val typename = "Boolean"
    def unapply(sample: String) = if (sample matches """^(yes|no|on|off|true|false)$""") Some(this) else None
}

abstract class VCollection[T <: Traversable[ast.Expression]] extends VType with Generic {
    val children: T
}

object VCollection extends VType {
    val parent = Some(VAny)
    val typename = "Collection"
}

case class VArray(val children: Seq[ast.Expression]) extends VCollection[Seq[ast.Expression]] {
    val parent = Some(VCollection)
    val types = children.map(_.vtype)
    val typename = "Array[" + types.mkString(", ") + "]"
    def unapply(sample: String) = if (sample matches """^\[[\w\W]*\]$""") Some(this) else None
}
