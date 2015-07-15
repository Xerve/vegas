package org.vegas

import org.scalatest._
import org.vegas.compiler.Compiler
import scala.io.Source

abstract class UnitTest extends FlatSpec
    with OptionValues
    with Matchers {
    def loadResource(resource: String) = Source.fromURL(getClass.getResource(resource)).mkString
}

abstract class FileMatchingTest(filePath: String, numTests: Int) extends UnitTest {
    val resources = (1 to numTests map (resrc =>
        s"/$filePath/testInput$resrc" -> s"/$filePath/expectedOutput$resrc"
    )).toMap

    def testFiles(compiler: Compiler) {
        resources foreach { case (input, output) =>
            val testInput = loadResource(input)
            val expectedOutput = loadResource(output)
            compiler.compile(testInput).value should equal (expectedOutput)
        }
    }
}
