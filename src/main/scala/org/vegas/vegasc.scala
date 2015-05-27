package org.vegas

import org.vegas.compiler.{Compiler, CompilerWriter, FileReader, FileWriter, PassThru}
import org.vegas.compiler.braces.{BracesCompiler, BracesFileCompiler}
import org.vegas.compiler.semicolon.{SemicolonCompiler, SemicolonFileCompiler}
import org.vegas.compiler.comment.{CommentCompiler, CommentFileCompiler}
import org.vegas.compiler.stageN.{StageNCompiler, StageNFileCompiler}

object vegasc {
    val version = "0.0.0"

    def main(args: Array[String]) {
        args.length match {
            case 0 => printHelp
            case 1 if args(0).startsWith("-") => parseFlag(args(0))
            case 1 => compileFile(args(0), Array())
            case 2 if args(0).startsWith("--stage-") => compileStage(args(0), args(1))
            case _ => compileFile(args(0), args.tail)
        }
    }

    def compileFile(filename: String, options: Array[String]) {
        def c(compiler: Compiler, switch: String) =
            if (options contains switch) PassThru() else compiler

        FileReader(filename) >>:
        c(CommentCompiler(), "--no-comment") >>:
        c(BracesCompiler(), "--no-braces") >>:
        c(SemicolonCompiler(), "--no-semicolons") >>:
        c(StageNCompiler(), "--no-parse") >>:
        FileWriter(filename)
    }

    def compileStage(stage: String, filename: String) {
        stage.stripPrefix("--stage-") match {
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
            case "-h" | "--help" => printHelp
            case _ => println(s"Unknown flag: $flag")
        }

    def printVersion =
        println(s"Vegas Compiler $version")

    def printHelp =
        println(s"Vegas Compiler $version\n" +
            "Usage: vegasc [opt] [filename]\n" +
            "opts:\n" +
            "  --stage2  => use the second stage compiler\n" +
            "              outputs filename.stage2.vegas\n" +
            "\n" +
            "  -h\n" +
            "  --help    => print this message\n" +
            "\n" +
            "  -v\n" +
            "  --version => prints the version number"
        )
}
