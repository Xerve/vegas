package org.vegas

import org.vegas.compiler.{Compiler, CompilerWriter, FileReader, FileWriter, PassThru}
import org.vegas.compiler.stage1.{Stage1Compiler, Stage1FileCompiler}
import org.vegas.compiler.stage2.{Stage2Compiler, Stage2FileCompiler}
import org.vegas.compiler.stageN.{StageNCompiler, StageNFileCompiler}

object vegasc {
    val version = "0.0.0"

    def main(args: Array[String]) {
        args.length match {
            case 0 => printHelp
            case 1 if args(0).startsWith("-") => parseFlag(args(0))
            case 1 => compileFile(args(0), Array())
            case 2 if args(0).startsWith("-") => compileStage(args(0), args(1))
            case 2 => compileFile(args(0), args.tail)
            case _ => println("Too many parameters specified!")
        }
    }

    def compileFile(filename: String, options: Array[String]) {
         FileReader(filename) >>:
         (if (options contains "--no-tags") PassThru() else Stage1Compiler()) >>:
         (if (options contains "--no-semicolons") PassThru() else Stage2Compiler()) >>:
         (if (options contains "--no-parse") PassThru() else StageNCompiler()) >>:
         FileWriter(filename)
    }

    def compileStage(stage: String, filename: String) {
        stage.stripPrefix("--") match {
            case "stage1" => Stage1FileCompiler(filename).compileToFile
            case "stage2" => Stage2FileCompiler(filename).compileToFile
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
