package org.vegas.vtype

import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

abstract class VMacro {
    val name: String
    def eval(callee: ast.Expression, args: Seq[ast.Expression]): ast.Expression
    override def toString = s"Macro<$name>"
}

case class GenericVMacro(val name: String, val function: (ast.Expression, Seq[ast.Expression]) => ast.Expression) extends VMacro {
    def eval(callee: ast.Expression, args: Seq[ast.Expression]) = function(callee, args)
}

abstract class VType {
    val parent: Option[VType]
    val typename: String
    val macros: Map[String, VMacro] = Map()

    def define(_macro: VMacro) {
        macros += _macro.name -> _macro
    }

    def define(macroName: String, function: (ast.Expression, Seq[ast.Expression]) => ast.Expression) {
        macros += macroName -> GenericVMacro(macroName, function)
    }

    def call(macroName: String, callee: ast.Expression, args: Seq[ast.Expression]): Option[ast.Expression] =
        macros get macroName match {
            case Some(vmacro) => Some(vmacro.eval(callee, args))
            case None => parent match {
                case Some(parent) => parent.call(macroName, callee, args)
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
