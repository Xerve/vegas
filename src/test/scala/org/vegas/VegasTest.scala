package org.vegas

import org.scalatest._
import org.vegas.compiler.Compiler
import scala.io.Source

abstract class UnitTest extends FlatSpec
    with OptionValues
    with Matchers {
    def resource(resource: String) = Source.fromURL(getClass.getResource(resource)).mkString
}
