package org.vegas.compiler

import org.scalatest._
import org.vegas.UnitTest
import org.vegas.compiler.braces.BracesCompiler

class BracesCompilerSpec extends UnitTest {
    "BracesCompiler" should "replace line ends with a brace" in {
        val input = """if this
                      |    do that""".stripMargin

        val output = """if this {
                       |    do that
                       |}""".stripMargin

        BracesCompiler(input) should equal (output)
    }
}
