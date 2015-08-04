package org.vegas

import scala.collection.mutable.Queue

object log {
    val log: Queue[Tuple2[String, String]] = new Queue()

    def apply(code: String, msg: String = "") {
        log += code -> msg
    }

    override def toString =
        "Log:\n" +
        log.map({ case (code, msg) =>
            if (msg != "") s"$code ->\n    $msg" else code
        }).mkString("\n")
}
