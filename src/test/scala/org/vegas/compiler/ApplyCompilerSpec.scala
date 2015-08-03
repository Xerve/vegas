package org.vegas.compiler

import org.scalatest._
import org.vegas.UnitTest
import org.vegas.compiler.apply.ApplyCompiler

class ApplyCompilerSpec extends UnitTest {
    "ApplyCompiler" should "replace a pattern with $apply" in {
        val input = """a.run(7)
                      |a run 7""".stripMargin

        val output = """a.run $apply (7)
                       |a run 7""".stripMargin

        ApplyCompiler(input) should equal (output)
    }
}
