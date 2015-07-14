package org.vegas.compiler

import org.scalatest._
import org.vegas.UnitTest
import org.vegas.compiler.apply.ApplyCompiler

class ApplyCompilerSpec extends UnitTest {
    val resources = (1 to 1 map (resrc => s"/ApplyCompiler/testInput$resrc" -> s"/ApplyCompiler/expectedOutput$resrc")).toMap

    "ApplyCompiler" should "replace a pattern with $apply" in {
        val compiler = ApplyCompiler()
        resources foreach { case (input, output) =>
            val testInput = loadResource(input)
            val expectedOutput = loadResource(output)
            println(expectedOutput)
            assert(compiler.compile(testInput), expectedOutput)
        }
    }
}
