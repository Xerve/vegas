package org.vegas.compiler

import org.scalatest._
import org.vegas.UnitTest

class SemicolonCompilerSpec extends UnitTest {
    "SemicolonCompiler" should "replace a semicolons to the end of lines" in {
        val input = """if this
                      |do a thing""".stripMargin

        val output = """if this;
                       |do a thing;""".stripMargin

        SemicolonCompiler(input) should equal (output)
    }

    it should "not add semicolons to line that end with excluded characters" in {
        val input = """this {
                      |this +
                      |this -
                      |this =
                      |this ,
                      |this [""".stripMargin

        SemicolonCompiler(input) should equal (input + ';')
    }
}
