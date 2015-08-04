package org.vegas

import scala.collection.mutable.Map

case class ProgramOptions(val name: String, val version: String, val args: Array[String]) {
    val shortFlag = """-(\w)"""
    val longFlag = """--([\w-]+)"""

    val parsedFlags = parseFlags(args.toList)

    val flags = parsedFlags.toMap map { case (key, value) =>
        key.getOrElse("") -> value
    } filterKeys (_ != "")

    val arguments = parsedFlags filter { case (flag, option) =>
        flag.isEmpty
    } map { case (key, value) =>
        value getOrElse ""
    }

    private def isFlag(arg: String) = (arg matches """^-(\w)$""") || (arg matches """^--([\w-]+)$""")

    private def parseFlags(program: List[String]): List[Tuple2[Option[String], Option[String]]] = program match {
        case Nil => Nil
        case flag :: Nil if isFlag(flag) => (Some(flag) -> None) :: Nil
        case argument :: Nil => (None -> Some(argument)) :: Nil
        case flag :: option :: tail if isFlag(flag) && !isFlag(option) => (Some(flag) -> Some(option)) :: parseFlags(tail)
        case flag :: tail if isFlag(flag) =>  (Some(flag) -> None) :: parseFlags(tail)
        case argument :: tail => (None -> Some(argument)) :: parseFlags(tail)
    }

    val aliases: Map[String, String] = Map()
    val reverseAliases: Map[String, String] = Map()
    val defaults: Map[String, String] = Map()
    val descriptions: Map[String, String] = Map()

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

    def apply(flag: String) = flags get flag

    def hasFlag(flag: String) = flags contains flag

    def printHelp { println(
        "Usage:\n" +
        descriptions.map { case (flag, text) =>
            (if (reverseAliases contains flag) ("    -" + reverseAliases.get(flag).get + "\n") else "") +
            "    --" + flag + "\n" +
            "        " + text + "\n"
        }.mkString
    )}

    def printVersion {
        println(s"$name $version")
    }
}
