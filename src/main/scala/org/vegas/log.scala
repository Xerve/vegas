package org.vegas

import scala.collection.mutable.Queue

object log {
    val _log: Queue[Tuple2[String, String]] = new Queue()

    def apply(code: String, msg: String = "") {
        _log += code -> msg
    }

    override def toString = _log.map({ case (code, msg) =>
        s"$code ->\n    $msg"
    }).mkString("\n")
}
