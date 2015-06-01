package org.vegas.compiler.comment

import org.vegas.compiler.{Compiler, FileCompiler}

class CommentCompiler extends Compiler {
    def compile(source: String) =
        Some(source.lines.map( line => {
            """^(\s{4})*#\[\w+\].*""".r
            .findFirstIn(line) match {
                case Some(x) => x
                case _ => {
                    var inString = false
                    var control = false

                    line.takeWhile(_ match {
                        case '"' if !control =>
                            inString = !inString
                            control = false
                            true
                        case '\\' if !control =>
                            control = true
                            true
                        case '#' if !inString && !control => false
                        case _ =>
                            control = false
                            true
                    }).stripSuffix(" ")
                }
            }
        }).mkString("\n"))
}

object CommentCompiler {
    implicit lazy val compiler = new CommentCompiler()
    def apply() = compiler
}

case class CommentFileCompiler(val filename: String)
extends FileCompiler[CommentCompiler]("comment")
