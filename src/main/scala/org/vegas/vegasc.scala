package org.vegas

import org.vegas.compiler.{Compiler, CompilerOptions, FileReader, FileWriter, PassThru, StdOut}
import org.vegas.compiler.stringReplace.{StringLoader, StringUnloader}
import org.vegas.compiler.apply.{ApplyCompiler, ApplyFileCompiler}
import org.vegas.compiler.braces.{BracesCompiler, BracesFileCompiler}
import org.vegas.compiler.semicolon.{SemicolonCompiler, SemicolonFileCompiler}
import org.vegas.compiler.comment.{CommentCompiler, CommentFileCompiler}
import org.vegas.compiler.stageN.{StageNCompiler, StageNFileCompiler}
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
        case 1 => compileFile(args(0))
        case 2 if args(0).startsWith("--stage-") => compileStage(args(0), args(1))
        case _ => compileFile(args(0))
    }

    def compileFile(filename: String) {
        def c(compiler: Compiler, switch: String) =
            if (options hasFlag switch) PassThru else compiler

        FileReader(filename) >>:
        c(StringLoader(), "--no-strings") >>:
        c(ApplyCompiler(), "--no-apply") >>:
        c(CommentCompiler(), "--no-comment") >>:
        c(BracesCompiler(), "--no-braces") >>:
        c(SemicolonCompiler(), "--no-semicolons") >>:
        c(StringUnloader(), "--no-strings") >>:
        c(StageNCompiler(), "--no-stageN") >>:
        (if (options hasFlag "--stdout") StdOut else FileWriter(filename))

        if (options hasFlag "--debug") {
            println(Compiler.types)
            println("")
            println(log)
        }
    }

    def compileStage(stage: String, filename: String) {
        stage.stripPrefix("--stage-") match {
            case "apply" => ApplyFileCompiler(filename).compileToFile
            case "comment" => CommentFileCompiler(filename).compileToFile
            case "braces" => BracesFileCompiler(filename).compileToFile
            case "semicolon" => SemicolonFileCompiler(filename).compileToFile
            case "stageN" => StageNFileCompiler(filename).compileToFile
            case _ => println(s"Cannot find compiler stage $stage")
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
