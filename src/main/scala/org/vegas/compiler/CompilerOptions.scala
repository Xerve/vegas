package org.vegas.compiler

import scala.collection.mutable.Map

case class CompilerOptions(val args: Array[String]) {
    val shortFlag = """-(\w)""".r
    val longFlag = """--([\w-]+)""".r

    val aliases: Map[String, String] = Map()
    val reverseAliases: Map[String, String] = Map()
    val defaults: Map[String, String] = Map()
    val descriptions: Map[String, String] = Map()
    lazy val flags: Map[String, String] = parseFlags()

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

    def parseFlags() = args.foldLeft(Tuple2[Map[String, String], Option[String]](Map(), None)) { (f, arg) =>
        f._2 match {
            case Some(setFlag) => (f._1 + (setFlag -> (arg match {
                case _ if arg.startsWith("-") => defaults.get(setFlag) match {
                    case Some(default) => default
                    case None => ""
                }
                case _ => arg
            }))) -> None
            case None => f._1 -> Some(arg match {
                case shortFlag(flag) => aliases.get(flag) match {
                    case Some(alias) => alias
                    case None => flag
                }
                case longFlag(flag) => flag
                case _ => ""
            })
        }
    }._1

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
