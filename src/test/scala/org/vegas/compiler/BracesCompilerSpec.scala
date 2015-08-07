package org.vegas.compiler

import org.scalatest._
import org.vegas.UnitTest

class BracesCompilerSpec extends UnitTest {
    "BracesCompiler" should "replace a add braces on indentation" in {
        val input = """if this
                      |    do a thing""".stripMargin

        val output = """if this {
                       |    do a thing
                       |}""".stripMargin

        BracesCompiler(input) should equal (output)
    }

    it should "do nothing if the line ends with a [ or ]" in {
        val input = """if this [
                      |    do a thing
                      |]""".stripMargin

        BracesCompiler(input) should equal (input)
    }

    it should "throw an exception with unexpected indentation" in {
        val input = """if this
                      |        bad indentation""".stripMargin

        an [exception.UnexpectedIndentationException] should be thrownBy BracesCompiler(input)
    }
}
