package org.vegas

import org.vegas.compiler.{Compiler, FileReader, FileWriter, PassThru, StdOut}
import org.vegas.compiler.{StringLoader, StringUnloader}
import org.vegas.compiler.ApplyCompiler
import org.vegas.compiler.BracesCompiler
import org.vegas.compiler.SemicolonCompiler
import org.vegas.compiler.CommentCompiler
import org.vegas.compiler.StageNCompiler
import org.vegas.prelude.Prelude
import org.vegas.vtype.ast

object vegasc extends App {
    val name = "Vegas Compiler"
    val version = "0.1.0"

    val options = ProgramOptions(name, version, args)
                  .alias("v", "version")
                  .description("version", "Prints the version number")
                  .alias("h", "help")
                  .description("help", "Prints help")

    Prelude.init

    if (options hasFlag "-v") options.printVersion
    if (options hasFlag "-h") options.printHelp
    if (args.isEmpty) options.printHelp else options.arguments foreach (compileFile(_))

    if (options hasFlag "--debug") {
        println(Compiler.scope)
        println("")
        println(log)
    }

    def compileFile(filename: String) {
        def c(compiler: Compiler, switch: String) =
            if (options hasFlag "--just") {
                if (options hasFlag "--" + switch) compiler else PassThru
            } else {
                if (options hasFlag "--no=" + switch) PassThru else compiler
            }

        FileReader(filename) >>:
        c(StringLoader(), "strings") >>:
        c(ApplyCompiler(), "apply") >>:
        c(CommentCompiler(), "comment") >>:
        c(BracesCompiler(), "braces") >>:
        c(SemicolonCompiler(), "semicolons") >>:
        c(StringUnloader(), "strings") >>:
        c(StageNCompiler(), "stageN") >>:
        (if (options hasFlag "--stdout") StdOut else FileWriter(filename))
    }
}
