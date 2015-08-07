package org.vegas.compiler

package object exception {
    abstract class VegasException(val msg: String) extends RuntimeException(msg) {
        val description: String
    }

    case class UnexpectedIndentationException(override val msg: String) extends VegasException(msg) {
        val description = """There was an unexpected indentation somewhere in your code.
                            |This usually happens if you use the wrong indentation style.
                            |The only allowed indentation in 4 spaces""".stripMargin
    }
}
