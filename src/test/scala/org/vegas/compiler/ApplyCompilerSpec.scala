package org.vegas.compiler

import org.scalatest._
import org.vegas.FileMatchingTest
import org.vegas.compiler.apply.ApplyCompiler

class ApplyCompilerSpec extends FileMatchingTest("ApplyCompiler", 1) {
    "ApplyCompiler" should "replace a pattern with $apply" in testFiles(ApplyCompiler())
}
