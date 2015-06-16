package org.vegas.compiler

import scala.collection.mutable.Map

case class CompilerOptions(val args: Array[String]) {
    val shortFlag = """-(\w)"""
    val longFlag = """--([\w-]+)"""

    val aliases: Map[String, String] = Map()
    val reverseAliases: Map[String, String] = Map()
    val defaults: Map[String, String] = Map()
    val descriptions: Map[String, String] = Map()
    lazy val flags: scala.collection.immutable.Map[String, Option[String]] = parseFlags()

    protected def chain(f: => Unit) = {
        f
        this
    }

    def alias(short: String, long: String) = chain {
        aliases += (short -> long)
        reverseAliases += (long -> short)
    }

    def default(flag: String, value: String) = chain {
        defaults += (flag -> value)
    }

    def description(flag: String, desc: String) = chain {
        descriptions += (flag -> desc)
    }

    def parseFlags() = {
        def isFlag(arg: String) = (arg matches """-(\w)""") || (arg matches """--([\w-]+)""")

        val flags = (for (arg <- args.sliding(2)
            if isFlag(arg.head)
        ) yield arg.lastOption match {
            case Some(flag) => arg.head -> (if (isFlag(flag)) None else Some(flag))
            case None => arg.head -> None
        }).toMap

        flags + (args.last -> None)
    }

    def apply(flag: String) = flags get flag

    def hasFlag(flag: String) = flags contains flag

    override def toString = "Options given:\n    " + flags.mkString("\n    ")

    def printHelp { println(
        "Usage:\n" +
        descriptions.map { case (flag, text) =>
            (if (reverseAliases contains flag) ("    -" + reverseAliases.get(flag).get + "\n") else "") +
            "    --" + flag + "\n" +
            "        " + text + "\n"
        }.mkString
    )}
}
