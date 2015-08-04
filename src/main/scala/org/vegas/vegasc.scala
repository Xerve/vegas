package org.vegas

import org.vegas.compiler.{Compiler, CompilerOptions, FileReader, FileWriter, PassThru, StdOut}
import org.vegas.compiler.{StringLoader, StringUnloader}
import org.vegas.compiler.ApplyCompiler
import org.vegas.compiler.BracesCompiler
import org.vegas.compiler.SemicolonCompiler
import org.vegas.compiler.CommentCompiler
import org.vegas.compiler.StageNCompiler
import org.vegas.prelude.Prelude
import org.vegas.vtype.ast

object vegasc extends App {
    val version = "0.0.0"

    val options = CompilerOptions(args)
                  .alias("v", "version")
                  .description("version", "Prints the version number")
                  .alias("h", "help")
                  .description("help", "Prints help")

    Prelude.init

    args.length match {
        case 0 => options.printHelp
        case 1 if args(0).startsWith("-") => parseFlag(args(0))
        case _ => compileFile(args(0))
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

        if (options hasFlag "--debug") {
            println(Compiler.scope)
            println("")
            println(log)
        }
    }

    def parseFlag(flag: String) =
        flag match {
            case "-v" | "--version" => printVersion
            case "-h" | "--help" => options.printHelp
            case _ => println(s"Unknown flag: $flag")
        }

    def printVersion =
        println(s"Vegas Compiler $version")
}
