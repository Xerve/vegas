package org.vegas

import org.scalatest.{FlatSpec, OptionValues}
import scala.io.Source

abstract class UnitTest extends FlatSpec with OptionValues {
    def loadResource(resource: String) = Source.fromURL(getClass.getResource(resource)).toString()
}
