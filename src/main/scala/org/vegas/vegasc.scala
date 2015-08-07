package org.vegas

import org.vegas.compiler._
import org.vegas.prelude.Prelude
import org.vegas.vtype.ast

object vegasc extends App {
    val name = "Vegas Compiler"
    val version = "0.1.0"

    val options = ProgramOptions(name, version, args)
                  .description("version", "Prints the version number")
                  .description("help", "Prints help")
                  .description("stdout", "Print output, rather than in a file")
                  .description("debug", "Print internal compiler information")

    Prelude.init

    if (options hasFlag "-v") options.printVersion
    if (options hasFlag "-h") options.printHelp
    if (args.isEmpty) options.printHelp else options.arguments foreach (compileFile(_))

    if (options hasFlag "--debug") {
        println("")
        println(Compiler.scope)
        println("")
        println(log)
    }

    def compileFile(filename: String) {
        def c(compiler: Compiler, switch: String) =
            if (options hasFlag "--just") {
                if (options hasFlag "--" + switch) compiler else PassThru
            } else if (options hasFlag "--no") {
                if (options hasFlag "--" + switch) PassThru else compiler
            } else {
                compiler
            }

        FileReader(filename) >>:
        c(StringLoader(), "strings") >>:
        c(ApplyCompiler(), "apply") >>:
        c(BracesCompiler(), "braces") >>:
        c(SemicolonCompiler(), "semicolons") >>:
        c(StringUnloader(), "strings") >>:
        c(StageNCompiler(), "stageN") >>:
        (if (options hasFlag "--stdout") StdOut else FileWriter(filename))
    }
}
