package org.vegas.compiler

abstract class VType {
    val typename: String
}

trait Generic {
    self: VType =>

    val types: Seq[VType]
}

object VAny extends VType {
    val typename = "Any"
    def unapply(sample: String) = Some(this)
}

object VNull extends VType {
    val typename = "Null"
    def unapply(sample: String) = if (sample == "null") Some(this) else None
}

object VNumber extends VType {
    val typename = "Number"
    def unapply(sample: String) = if (sample matches """^-?\d+\.?$""") Some(this) else None
}

object VString extends VType {
    val typename = "String"
    def unapply(sample: String) = if (sample matches """"(?:\\.|[^"\\])*""") Some(this) else None
}

object VBoolean extends VType {
    val typename = "Boolean"
    def unapply(sample: String) = if (sample matches """^(yes|no|on|off|true|false)$""") Some(this) else None
}

case class VArray(val types: Seq[VType]) extends VType with Generic {
    val typename = "Array[" + types.mkString(", ") + "]"
    def unapply(sample: String) = if (sample matches """^\[[\w\W]*\]$""") Some(this) else None
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
